;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.pool
  "Clojure wrapper around worker-router/WorkerPool.

   Library contexts live as gc-mode resource-tracker owners, whose
   disposefn fires when V8 collects the owner. The bounded-LRU helpers
   enforce max-live-ctxs by evicting idle contexts ahead of GC."
  (:require ["worker-router" :as cp]
            ["resource-tracker" :as resource]
            ["./handler_runtime.mjs" :as hrt]
            [clojure.string :as string]))

;; The `disp-id` of the dispatch trace events, so a reader can pair each start
;; with its end across reordered worker output. Distinct from the per-handler
;; `id` of handler-runtime.
(defonce ^:private dispatch-id-counter (atom 0))

(defn- sanitize-reason
  "Make `msg` safe for a key=value trace field: whitespace and `=` runs
   become `_`, capped at 200 characters."
  [msg]
  (let [s (if (string? msg) msg (str msg))
        one-line (string/replace s #"[\s=]+" "_")]
    (if (> (count one-line) 200) (subs one-line 0 200) one-line)))

(defn ^:async worker-call
  "Call `method-name` on handler `handler-key`, and return the awaited
   result. A nil worker-idx routes through pool.any(), the least-loaded
   worker. An integer routes through pool.worker(idx).

   `args` is a JS array or nil. It spreads through the host
   Function.prototype.apply, because squint `apply` can throw `Cannot
   convert object to primitive value` on a Comlink proxy.

   No refcount here, because lifecycle ops must bypass it. call! adds
   ref-handle! and unref-handle! around each ccall."
  [pool handler-key method-name args worker-idx]
  (let [target         (if (some? worker-idx)
                         (.worker pool worker-idx)
                         (.any pool))
        handler-name   (str handler-key)
        method-fn      (aget (aget target handler-name) method-name)
        call-args      (or args #js [])
        ;; Read once, so the open and close events pair even when
        ;; setLogConfig changes the level during the call.
        queue-enabled? (hrt/isEnabled "QUEUE-DISPATCH")
        rpc-enabled?   (hrt/isEnabled "RPC-POST")
        ev             (when (or queue-enabled? rpc-enabled?)
                         #js {:lib handler-name
                              :fn method-name
                              :c-fn (when (= method-name "ccall") (aget call-args 0))
                              :worker (if (some? worker-idx) worker-idx "auto")
                              :disp-id (swap! dispatch-id-counter inc)})]
    (hrt/dbgPaired queue-enabled? "QUEUE-DISPATCH" ev)
    (hrt/dbgPaired rpc-enabled? "RPC-POST" ev)
    (try
      (let [result (await (.apply method-fn nil call-args))]
        (hrt/dbgPaired rpc-enabled? "RPC-REPLY" ev)
        (hrt/dbgPaired queue-enabled? "QUEUE-COMPLETE" ev)
        result)
      (catch :default e
        (when ev
          (aset ev "failed" "true")
          (aset ev "reason" (sanitize-reason (or (some-> e .-message) (str e)))))
        (hrt/dbgPaired rpc-enabled? "RPC-REPLY" ev)
        (hrt/dbgPaired queue-enabled? "QUEUE-COMPLETE" ev)
        (throw e)))))

(defn- ^:async broadcast-to-handlers!
  "Call `method-name` on every handler key of every worker, with the args
   that (args-fn worker-idx) returns. Every key gets the call, because each
   handler module can carry its own handler_runtime state. Each call
   swallows its own error, so a handler not built on makeHandler, which
   has no such method, does not fail pool init."
  [pool handler-keys method-name args-fn]
  (dotimes [w (.-size pool)]
    (let [target (.worker pool w)]
      (doseq [k handler-keys]
        (try
          (let [method-fn (aget (aget target k) method-name)]
            (await (.apply method-fn nil (args-fn w))))
          (catch :default _e nil))))))

(defn- log-config
  "`opts` with :categories as an array, which setLogConfig and a structured
   clone take. A caller can pass any collection."
  [opts]
  (cond-> opts (some? (:categories opts)) (update :categories vec)))

(defn ^:async init-pool!
  "Spawn a WorkerPool that loads `handlers`, handler-key -> {:module url
   :init args}. :size is \"auto\" (default) or an integer. Each worker gets
   its slot index, and the :handler-runtime log config when there is one.
   Returns the pool."
  [{:keys [handlers size handler-runtime]}]
  (let [pool         (await (.create (.-WorkerPool cp)
                                     #js {:size (or size "auto")
                                          :bootstrap (.resolve js/import.meta "worker-router/worker-bootstrap")
                                          :handlers handlers}))
        handler-keys (vec (js/Object.keys handlers))]
    (when (some? handler-runtime)
      (await (broadcast-to-handlers! pool handler-keys "__setLogConfig"
                                     (fn log-config-args [_w] #js [(log-config handler-runtime)]))))
    (await (broadcast-to-handlers! pool handler-keys "__setWorkerSlot"
                                   (fn worker-slot-args [w] #js [w])))
    pool))

(defn pool-size
  "The worker count of `pool`."
  [pool]
  (.-size pool))

(defn set-log-config!
  "Configure host-side trace logging, which is off by default. A nil opts
   turns it off. An absent key keeps its current state.

   opts:
     :level       :off, :error, :warn, :info, :debug or :trace. nil is off.
                  Every current event is debug level.
     :categories  Keywords or strings to allow, nil for all. A category is
                  the lower-case event-tag prefix: BUSY-* is :busy.

   Workers take the same config through init-args
   {:handlerRuntime {:logLevel :logCategories}}.

   (set-log-config! {:level :debug :categories [:busy :fr]})"
  [opts]
  (hrt/setLogConfig (log-config opts)))

;; A consumer keeps each native handle (a CURL*, a sqlite3*) on the worker
;; that made it. claim() picks the least-loaded worker and counts the claim in
;; one step, so no dispatch can race the pick.

(defn claim
  "Reserve the least-loaded worker through worker-router claim(). Returns
   the raw #js {index, release}. assign-worker-for-context! wraps it."
  [pool]
  (.claim pool))

(def ^:private DEFAULT-MIN-AGE-MS 100)

(defn- now-ms [] (.now js/Date))

;; Dispose Promises for flush-pending-disposes!. Kept here so resource-tracker
;; stays a port of tech.resource.
(defonce ^:private pending-disposes (js/Set.))

;; parent-ctx-id -> count of child handles registered with this parent whose
;; release has not settled. A live, undisposed child counts. A parent destroy
;; waits for zero. A membership poll is not enough: an entry leaves
;; live-handles before its worker_call posts.
(defonce ^:private in-flight-by-parent (atom {}))

;; parent-ctx-id -> {:promise :resolve}, resolved when in-flight reaches zero.
;; A parent destroy awaits it, because a setTimeout-0 poll starves behind
;; worker-queue traffic.
(defonce ^:private drain-promises-by-parent (atom {}))

(defn await-parent-drain!
  "A Promise that resolves when the in-flight count of `parent` is zero.
   Check the count again after it resolves, because a new register-handle!
   can raise it first."
  [parent]
  (if (zero? (get @in-flight-by-parent parent 0))
    (js/Promise.resolve nil)
    (:promise
     (or (get @drain-promises-by-parent parent)
         (let [resolve-fn (atom nil)
               promise    (js/Promise. (fn [resolve _reject] (reset! resolve-fn resolve)))
               entry      {:promise promise :resolve @resolve-fn}]
           (swap! drain-promises-by-parent assoc parent entry)
           entry)))))

(defn- capture-pending-dispose!
  "When `result` is a Promise, add it to pending-disposes. Returns
   `result`."
  [result]
  (when (instance? js/Promise result)
    (.add pending-disposes result)
    ;; A fulfilled dispose leaves at once, since a page may flush only at
    ;; shutdown. A rejected one stays, so the flush reports it.
    (.then result (fn [_] (.delete pending-disposes result)) (fn [_] nil)))
  result)

(defn flush-pending-disposes!
  "Promise.allSettled over the captured disposer Promises that are pending,
   or that rejected since the last flush. Clears the list first, so a
   concurrent dispose goes to the next flush. Await it before you terminate
   the workers, or an async destroy can fail to reach them."
  []
  (let [pending (.from js/Array pending-disposes)]
    (.clear pending-disposes)
    (.allSettled js/Promise pending)))

(defn fire-and-capture-dispose!
  "Call `disposer-fn` and capture its Promise, so an explicit
   (Symbol.dispose) release drains through flush-pending-disposes! like a GC
   release. Traces `context-info` as EXPLICIT-DISPOSE."
  [disposer-fn context-info]
  (hrt/dbg "EXPLICIT-DISPOSE" (or context-info #js {}))
  (capture-pending-dispose! (disposer-fn)))

;; library-key ->
;;   {:ctx-workers (atom {ctx-id -> {:release Fn}})
;;    :live-handles    (atom {ctx-id -> {:ctx-id :owner WeakRef :release Fn
;;                                   :created-at ms :touched-at ms
;;                                   :refcount Integer}})
;;    :max-live-ctxs (Integer | nil)
;;    :min-age-ms   Integer
;;    :evicted (atom #{ctx-id ...})   ; LRU-reclaimed, tombstoned
;;    :stats (atom {:evictions Integer :blocks Integer})}
;;
;; ctx-workers holds claim releases. live-handles holds bounded-LRU entries.
;; A ctx-id can be in both.
(defonce ^:private library-contexts (atom {}))

(defn register-library-context!
  "Set up the state of `library-key` once, and return it. A later call sets
   the options that it gives.

   opts:
     :max-live-ctxs  The live-handle bound. nil means none.
     :min-age-ms     The eviction age gate. Default 100."
  ([library-key] (register-library-context! library-key nil))
  ([library-key opts]
   (when-not (contains? @library-contexts library-key)
     (swap! library-contexts assoc library-key
            {:ctx-workers (atom {})
             :live-handles (atom {})
             :max-live-ctxs nil
             :min-age-ms DEFAULT-MIN-AGE-MS
             :evicted (atom #{})
             :stats (atom {:evictions 0 :blocks 0})}))
   (let [{:keys [max-live-ctxs min-age-ms]} opts]
     (when (some? max-live-ctxs)
       (swap! library-contexts assoc-in [library-key :max-live-ctxs] max-live-ctxs))
     (when (some? min-age-ms)
       (swap! library-contexts assoc-in [library-key :min-age-ms] min-age-ms)))
   (get @library-contexts library-key)))

(defn worker-idx-from-args
  "The first .worker_idx of an object in `args`, else 0. The 0 default pins
   unrouted calls to one worker, because each worker's wasm module holds its
   own state, such as MEMFS files and driver registries, and this layer
   cannot tell a pure call from one that touches it. To spread a known-pure
   call, call worker-call with a nil worker-idx."
  [args]
  (or (some (fn worker-idx-of [arg]
              (when (object? arg) (.-worker-idx arg)))
            args)
      0))

(defn assign-worker-for-context!
  "Pick a worker for a new context. Returns {:idx :release}. An explicit
   :worker is bounds-checked and takes no claim. Without it, pool.claim()
   picks the least-loaded worker.

   Call release exactly once, on destroy or on create failure, so the
   claim_count of the pool drains."
  [pool _library-key opts]
  (if-let [explicit (:worker opts)]
    (let [worker-count (pool-size pool)]
      (when (or (neg? explicit) (>= explicit worker-count))
        (throw (js/Error. (str "Worker index " explicit " out of range (max " (dec worker-count) ")"))))
      {:idx explicit :release (fn [] nil)})
    (let [c (claim pool)]
      {:idx (.-index c) :release (.-release c)})))

(defn- release-on-gc!
  "Track `owner`, and return a release fn that V8 also calls when it
   collects `owner`. A CAS makes a GC and an explicit call run `on-release`
   once between them. `on-release` must not close over `owner`, or the owner
   stays reachable."
  [library-key ctx-id kind worker owner on-release]
  (let [fired?  (atom false)
        release (fn []
                  (if (compare-and-set! fired? false true)
                    (do (hrt/dbg "FR-CALLBACK" #js {:lib (str library-key) :ctx-id ctx-id :kind kind :worker worker})
                        (on-release))
                    (hrt/dbg "FR-CALLBACK-SUPPRESSED" #js {:lib (str library-key) :ctx-id ctx-id :kind kind})))]
    (resource/track owner #js {:tracktype "gc" :disposefn release})
    (hrt/dbg "FR-TRACK" #js {:lib (str library-key) :ctx-id ctx-id :kind kind :worker worker})
    release))

(defn track-context!
  "Record ctx-id -> {:release}, and track `owner` so release-fn fires
   when V8 collects it. release-fn must not close over `owner`. An explicit
   untrack plus a later GC fire one release. `ctx-id` must be unique within
   the library, or a later track replaces the earlier entry."
  [library-key ctx-id worker-idx release-fn owner]
  (let [ctx-workers (:ctx-workers (register-library-context! library-key))
        release (release-on-gc! library-key ctx-id "ctx" worker-idx owner
                                (fn []
                                  (swap! ctx-workers dissoc ctx-id)
                                  (capture-pending-dispose! (release-fn))))]
    (swap! ctx-workers assoc ctx-id {:release release})))

(defn untrack-context!
  "Fire the stored release of ctx-id, which drains its pool claim.
   Idempotent, and a no-op for an untracked ctx-id."
  [library-key ctx-id]
  (when-let [entry (get @library-contexts library-key)]
    (when-let [release (:release (get @(:ctx-workers entry) ctx-id))]
      (release))))

(defn reset-library-context!
  "Consumer shutdown. Fire every stored claim release and LRU disposer, then
   clear ctx-workers, live-handles and the eviction tombstones. The CAS on
   each release makes a later GC fire a no-op."
  [library-key]
  (when-let [entry (get @library-contexts library-key)]
    (doseq [a [(:ctx-workers entry) (:live-handles entry)]]
      (doseq [[_ stored] @a]
        (when-let [release (:release stored)]
          (release)))
      (reset! a {}))
    (reset! (:evicted entry) #{})))

(defn- dec-in-flight!
  "Count down the in-flight children of `parent-ctx-id`, and resolve its
   drain promise at zero."
  [parent-ctx-id]
  (let [m (swap! in-flight-by-parent
                 (fn [m]
                   (let [c (get m parent-ctx-id 0)]
                     (if (<= c 1)
                       (dissoc m parent-ctx-id)
                       (assoc m parent-ctx-id (dec c))))))]
    (when (zero? (get m parent-ctx-id 0))
      (when-let [entry (get @drain-promises-by-parent parent-ctx-id)]
        (swap! drain-promises-by-parent dissoc parent-ctx-id)
        ((:resolve entry) nil)))))

(defn register-handle!
  "Track a handle in live-handles, and track `owner` so release-fn frees the
   native handle on GC. Returns ctx-id. release-fn must not close over
   `owner`. An eviction or manual release plus a later GC fire one release.

   With parent-ctx-id, in-flight-by-parent counts the handle until its
   release Promise settles. A parent destroy waits for that count to reach
   zero."
  ([library-key ctx-id exec-unit-idx release-fn owner]
   (register-handle! library-key ctx-id exec-unit-idx release-fn owner nil))
  ([library-key ctx-id exec-unit-idx release-fn owner parent-ctx-id]
   (let [lib  (register-library-context! library-key)
         live (:live-handles lib)
         done #(when (some? parent-ctx-id) (dec-in-flight! parent-ctx-id))
         release (release-on-gc! library-key ctx-id "handle" exec-unit-idx owner
                                 (fn []
                                   (swap! live dissoc ctx-id)
                                   (let [p (release-fn)]
                                     (capture-pending-dispose!
                                      (if (instance? js/Promise p)
                                        (.finally p done)
                                        (do (done) p))))))]
     (swap! live assoc ctx-id
            {:ctx-id ctx-id
             :owner (js/WeakRef. owner)
             :release release
             :created-at (now-ms)
             :touched-at (now-ms)
             :refcount 0})
     (swap! (:evicted lib) disj ctx-id)
     (when (some? parent-ctx-id)
       (swap! in-flight-by-parent
              (fn [m] (assoc m parent-ctx-id (inc (get m parent-ctx-id 0))))))
     ctx-id)))

(defn dispose-handle!
  "Fire the release of live handle ctx-id now, through the same wrapped
   disposer as GC, so the in-flight accounting runs. Returns release-fn's
   result (the dispose Promise), or undefined when the handle is gone and
   the caller must use its raw destroy-fn. release-fn must return a Promise,
   or the caller cannot tell the two apart. A raw destroy of a live handle
   never decrements in-flight-by-parent, and wedges a parent destroy."
  [library-key ctx-id]
  (when-let [lib (get @library-contexts library-key)]
    (when-let [release (:release (get @(:live-handles lib) ctx-id))]
      (release))))

(defn in-flight-count-for-parent
  "The count of child handles registered under `parent` whose release has
   not settled, live children included."
  [parent]
  (get @in-flight-by-parent parent 0))

(defn ref-handle!
  "Increment the refcount of ctx-id and update :touched-at. Pair it with
   unref-handle! in a finally. Eviction skips an entry with a refcount above
   0."
  [library-key ctx-id]
  (when-let [lib (get @library-contexts library-key)]
    (swap! (:live-handles lib)
           (fn [m]
             (if (contains? m ctx-id)
               (-> m
                   (update-in [ctx-id :refcount] inc)
                   (assoc-in [ctx-id :touched-at] (now-ms)))
               m)))))

(defn unref-handle!
  "Decrement the refcount of ctx-id, with a floor of 0. A no-op for an
   untracked ctx-id."
  [library-key ctx-id]
  (when-let [lib (get @library-contexts library-key)]
    (swap! (:live-handles lib)
           (fn [m]
             (if (contains? m ctx-id)
               ;; A count below zero would let the next ref-handle! leave an
               ;; in-flight entry at 0, where eviction can free it mid-call.
               (update-in m [ctx-id :refcount] (fn [rc] (max 0 (dec rc))))
               m)))))

;; find-oldest-evictable and get-pool-detail both use entry-evictable?, so the
;; diagnostic matches the behavior. Each takes one `now` per scan, so no entry
;; crosses the age boundary mid-scan.

(defn- entry-age-ms
  "The age of entry `e` at `now`, from :created-at, so a context that is
   touched often still ages out."
  [e now]
  (- now (:created-at e)))

(defn- entry-busy? [e] (pos? (:refcount e)))

(defn- entry-evictable?
  [e now min-age-ms]
  (and (not (entry-busy? e))
       (>= (entry-age-ms e now) min-age-ms)))

(defn- find-oldest-evictable
  "The least recently touched live entry that is idle and past min-age, or
   nil. Owner reachability does not count, so the cap reclaims idle
   contexts ahead of GC."
  [live min-age-ms]
  (let [now (now-ms)
        evictable (filter (fn [e] (entry-evictable? e now min-age-ms))
                          (vals live))]
    (when (seq evictable)
      (apply min-key (fn [e] (:touched-at e)) evictable))))

(defn evicted?
  "True when an LRU eviction reclaimed ctx-id and freed its native handle."
  [library-key ctx-id]
  (boolean (when-let [lib (get @library-contexts library-key)]
             (contains? @(:evicted lib) ctx-id))))

(defn evict-oldest!
  "Tombstone and release the least recently touched evictable entry.
   Returns \"evicted\", \"none-evictable\" or \"empty\". Does not update
   :evictions, which counts only bounded-create-handle! evictions."
  [library-key]
  (when-let [lib (get @library-contexts library-key)]
    (if-let [entry (find-oldest-evictable @(:live-handles lib) (:min-age-ms lib))]
      ;; The entry's own ctx-id, since a map key comes back a string. Before the
      ;; release, so a reuse fails on evicted? before it reaches freed memory.
      (do (swap! (:evicted lib) conj (:ctx-id entry))
          ((:release entry))
          "evicted")
      (if (empty? @(:live-handles lib)) "empty" "none-evictable"))))

(defn bounded-create-handle!
  "Run `create-fn`, which must call register-handle!, under the
   max-live-ctxs bound, and return its result. At the bound, evict the LRU
   idle entry first. When every entry is busy or below min-age, throw
   ex-info with {:blocked :bounded-blocked}."
  [library-key create-fn]
  (let [lib   (register-library-context! library-key)
        bound (:max-live-ctxs lib)
        live  (count @(:live-handles lib))]
    (when (and bound (>= live bound))
      (if (= "evicted" (evict-oldest! library-key))
        (swap! (:stats lib) update :evictions inc)
        (do (swap! (:stats lib) update :blocks inc)
            (throw (ex-info "live-handles at bound, no evictable entry"
                            {:library library-key
                             :live live
                             :max bound
                             :blocked :bounded-blocked})))))
    (create-fn)))

(defn get-pool-detail
  "Diagnostic. Why each live entry is or is not evictable, as #js {total,
   evictable, blocked_refcount, blocked_age_gate, sample}. `sample` holds up
   to 8 blocked entries as {ctx_id, refcount, owner_alive, age_ms,
   age_gated}. undefined for an unregistered library."
  [library-key]
  (when-let [lib (get @library-contexts library-key)]
    (let [min-age (:min-age-ms lib)
          now     (now-ms)
          live    (vals @(:live-handles lib))
          blocked (filterv (fn [e] (not (entry-evictable? e now min-age))) live)
          busy    (count (filterv entry-busy? blocked))]
      #js {:total (count live)
           :evictable (- (count live) (count blocked))
           :blocked_refcount busy
           ;; An idle entry is blocked only by the age gate.
           :blocked_age_gate (- (count blocked) busy)
           :sample (mapv (fn blocked-sample [e]
                           #js {:ctx_id (str (:ctx-id e))
                                :refcount (:refcount e)
                                :owner_alive (some? (.deref (:owner e)))
                                :age_ms (entry-age-ms e now)
                                :age_gated (< (entry-age-ms e now) min-age)})
                         (take 8 blocked))})))

(defn get-pool-stats
  "The counters of a library as #js {live, evictions, blocks, max_live_ctxs,
   min_age_ms}. undefined for an unregistered library."
  [library-key]
  (when-let [lib (get @library-contexts library-key)]
    (let [s @(:stats lib)]
      #js {:live (count @(:live-handles lib))
           :evictions (:evictions s)
           :blocks (:blocks s)
           :max_live_ctxs (:max-live-ctxs lib)
           :min_age_ms (:min-age-ms lib)})))
