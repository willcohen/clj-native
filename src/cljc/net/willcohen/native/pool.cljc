;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#?(:clj
   (ns net.willcohen.native.pool
     "JVM stub. The JVM uses GraalVM Polyglot, not a worker pool, so every
      fn here except set-log-config! throws. pool-stub-test checks that this
      surface matches the CLJS one by name and arity.")
   :cljs
   (ns net.willcohen.native.pool
     "Clojure wrapper around worker-router/WorkerPool.

      Library contexts live as gc-mode resource-tracker owners, whose
      disposefn fires when V8 collects the owner. The bounded-LRU helpers
      enforce max-live-ctxs by evicting idle contexts ahead of GC."
     (:require ["worker-router" :as cp]
               ["resource-tracker" :as resource]
               ["./handler_runtime.mjs" :as hrt]
               [clojure.string :as string])))

#?(:cljs
   (defn- handler-spec->js
     [spec]
     ;; A squint map is a plain JS object, so clj->js does nothing here.
     ;; Copy only the keys the worker reads.
     (let [obj (js-obj)]
       (when-let [m (:module spec)]
         (aset obj "module" m))
       (when (contains? spec :init)
         (aset obj "init" (:init spec)))
       obj)))

#?(:cljs
   (defn- handlers->js
     [handlers]
     ;; A squint map and a caller's JS object are both plain objects, so one
     ;; doseq walks either.
     (let [obj  (js-obj)
           put! (fn [k v]
                  (aset obj (str k) (handler-spec->js v)))]
       (doseq [[k v] handlers] (put! k v))
       obj)))

#?(:cljs
   (defn- coerce-size
     [size]
     (if (or (nil? size) (= :auto size)) "auto" size)))

#?(:cljs
   (defn- opts-get
     "Read `k` from opts by its snake_case name, then by its own name. A
      squint map is a plain JS object, so map? is true for a JS object too.
      The object? branch must come first, or every snake_case JS key reads
      as nil."
     [opts k]
     (cond
       (nil? opts) nil
       (object? opts) (let [n (str k)
                            js-key (string/replace n "-" "_")
                            v (aget opts js-key)]
                        (if (undefined? v)
                          (aget opts n)
                          v))
       (map? opts) (get opts k)
       :else nil)))

#?(:cljs
   (defn- coerce-cat
     ;; A squint keyword is a string, so both pass through unchanged.
     [x]
     (if (string? x) x (name x))))

#?(:cljs
   (defn ^:async broadcast-to-handlers!
     "Call `method-name` on every handler key of every worker, with the args
      that (args-fn worker-idx) returns. Every key gets the call, because
      each bundle in a joint pool can carry its own handler_runtime state.
      Each call swallows its own error, so a handler built against an older
      clj-native without the method does not fail pool init."
     [pool handler-keys method-name args-fn]
     (dotimes [w (.-size pool)]
       (let [target (.worker pool w)]
         (doseq [k handler-keys]
           (try
             (let [handler-proxy (aget target k)
                   method-fn     (aget handler-proxy method-name)]
               (await (.apply method-fn nil (args-fn w))))
             (catch :default _e nil)))))))

#?(:cljs
   (defn ^:async init-pool!
     "Adopt the caller's pool, or spawn one. Returns #js {:pool :owned},
      where owned is true only when this call made the pool.

      opts:
        :pool             Adopt this WorkerPool. No spawn.
        :handlers         handler-key -> {:module url :init args}, keyed by
                          keyword or string. Required without :pool.
        :size             :auto (default), \"auto\" or an integer.
        :bootstrap        Bootstrap URL. Defaults to
                          worker-router/worker-bootstrap.
        :handler-runtime  Diagnostic config sent to every worker through
                          __setLogConfig. Ignored on adoption."
     [opts]
     (let [caller-pool (opts-get opts :pool)]
       (if (some? caller-pool)
         ;; `owned`, not `owned?`: squint writes #js {:owned? ..} as "owned?"
         ;; but reads (.-owned? x) as .owned_QMARK_.
         #js {:pool caller-pool :owned false}
         (let [handlers (opts-get opts :handlers)
               _ (when (nil? handlers)
                   (throw (ex-info "init-pool! requires :handlers when :pool is absent"
                                   {:opts opts})))
               size      (coerce-size (opts-get opts :size))
               bootstrap (or (opts-get opts :bootstrap)
                             (.resolve js/import.meta "worker-router/worker-bootstrap"))
               cp-opts   (js-obj "size"      size
                                 "bootstrap" bootstrap
                                 "handlers"  (handlers->js handlers))
               pool      (await (.create (.-WorkerPool cp) cp-opts))
               hr        (opts-get opts :handler-runtime)
               handler-keys (vec (js/Object.keys handlers))]
           ;; Not through worker-call, which is a forward reference here and
           ;; adds queue trace noise.
           (when (some? hr)
             (let [level (opts-get hr :level)
                   cats  (opts-get hr :categories)
                   cfg   (js-obj)]
               (when (some? level)
                 (aset cfg "level" (coerce-cat level)))
               (when (some? cats)
                 (let [arr (array)]
                   (doseq [x cats] (.push arr (coerce-cat x)))
                   (aset cfg "categories" arr)))
               (await (broadcast-to-handlers! pool handler-keys "__setLogConfig"
                                              (fn log-config-args [_w] #js [cfg])))))
           (await (broadcast-to-handlers! pool handler-keys "__setWorkerSlot"
                                          (fn worker-slot-args [w] #js [w])))
           #js {:pool pool :owned true})))))

#?(:cljs
   ;; The `disp-id` of the dispatch trace events, so a reader can pair each
   ;; start with its end across reordered worker output. Distinct from the
   ;; per-handler `id` of handler-runtime.
   (defonce ^:private dispatch-id-counter (atom 0)))

#?(:cljs
   (defn- sanitize-reason
     "Make `msg` safe for a key=value trace field: whitespace and `=` runs
      become `_`, capped at 200 characters."
     [msg]
     (let [s (if (string? msg) msg (str msg))
           one-line (string/replace s #"[\s=]+" "_")]
       (if (> (count one-line) 200) (subs one-line 0 200) one-line))))

#?(:cljs
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
     (let [target        (if (some? worker-idx)
                           (.worker pool worker-idx)
                           (.any pool))
           handler-name  (str handler-key)
           handler-proxy (aget target handler-name)
           method-fn     (aget handler-proxy method-name)
           call-args     (or args #js [])
           worker-tag    (if (some? worker-idx) worker-idx "auto")
           c-fn          (when (= method-name "ccall") (aget call-args 0))
           ;; Read once, so the open and close events pair even when
           ;; setLogConfig changes the level during the call.
           queue-enabled? (hrt/isEnabled "QUEUE-DISPATCH" "debug")
           rpc-enabled?   (hrt/isEnabled "RPC-POST" "debug")
           disp-id       (when (or queue-enabled? rpc-enabled?)
                           (swap! dispatch-id-counter inc))]
       (hrt/dbgPaired queue-enabled? "QUEUE-DISPATCH" #js {:lib handler-name :fn method-name :c-fn c-fn :worker worker-tag :disp-id disp-id})
       (hrt/dbgPaired rpc-enabled?   "RPC-POST"       #js {:lib handler-name :fn method-name :c-fn c-fn :worker worker-tag :disp-id disp-id})
       (try
         (let [result (await (.apply method-fn nil call-args))]
           (hrt/dbgPaired rpc-enabled?   "RPC-REPLY"      #js {:lib handler-name :fn method-name :c-fn c-fn :worker worker-tag :disp-id disp-id})
           (hrt/dbgPaired queue-enabled? "QUEUE-COMPLETE" #js {:lib handler-name :fn method-name :c-fn c-fn :worker worker-tag :disp-id disp-id})
           result)
         (catch :default e
           (let [reason (sanitize-reason (or (some-> e .-message) (str e)))]
             (hrt/dbgPaired rpc-enabled?   "RPC-REPLY"      #js {:lib handler-name :fn method-name :c-fn c-fn :worker worker-tag :disp-id disp-id :failed "true" :reason reason})
             (hrt/dbgPaired queue-enabled? "QUEUE-COMPLETE" #js {:lib handler-name :fn method-name :c-fn c-fn :worker worker-tag :disp-id disp-id :failed "true" :reason reason}))
           (throw e))))))

#?(:cljs
   (defn ^:async terminate-pool!
     "Call pool.terminate(). Consumers tear down through the registry's
      shutdown-pool!, which skips an adopted pool."
     [pool]
     (await (.terminate pool))))

#?(:cljs
   (defn pool-size
     "The worker count of `pool`."
     [pool]
     (.-size pool)))

#?(:cljs
   (defn set-log-config!
     "Configure the host-side diagnostic substrate, which is off by default.
      A nil opts turns it off. An absent key keeps its current state.

      opts:
        :level       :off, :error, :warn, :info, :debug or :trace. nil is
                     off. Every current event is debug level.
        :categories  Keywords or strings to allow, nil for all. A category
                     is the lower-case event-tag prefix: BUSY-* is :busy.

      Workers take the same config through init-args
      {:handlerRuntime {:logLevel :logCategories}}.

      (set-log-config! {:level :debug :categories [:busy :fr]})"
     [opts]
     (if (nil? opts)
       (hrt/setLogConfig nil)
       (let [cfg (js-obj)]
         (when (contains? opts :level)
           (let [l (:level opts)]
             (aset cfg "level" (cond
                                 (nil? l)    nil
                                 (string? l) l
                                 :else       (name l)))))
         (when (contains? opts :categories)
           (let [c (:categories opts)]
             (aset cfg "categories"
                   (cond
                     (nil? c) nil
                     :else    (let [arr (array)]
                                (doseq [x c] (.push arr (coerce-cat x)))
                                arr)))))
         (hrt/setLogConfig cfg)))))

#?(:cljs
   (defonce ^:private cmd-args-registry (atom {})))

#?(:cljs
   (defn register-cmd-args!
     "Register `f`, which maps a cmd-map to #js positional args, for a
      library-specific op such as context_create."
     [op-name f]
     (swap! cmd-args-registry assoc op-name f)))

#?(:cljs
   (defn- ccall-args
     "The 5-element JS args of a ccall envelope. Other keys go in the
      trailing `extra` object for the handler."
     [cmd]
     (let [extra    (js-obj)
           std-keys #{:cmd :fn :returnType :argTypes :args}]
       (doseq [[k v] cmd]
         (when (and (some? v) (not (contains? std-keys k)))
           (aset extra (str k) v)))
       #js [(:fn cmd) (:returnType cmd) (:argTypes cmd) (:args cmd) extra])))

#?(:cljs
   (defn cmd-args
     "Translate a {:cmd ...} envelope to #js positional args for the handler
      method. \"ccall\" is built in. Other ops use their register-cmd-args!
      translator, and an unknown op gives #js []."
     [cmd]
     (let [op (:cmd cmd)]
       (cond
         (= op "ccall") (ccall-args cmd)
         :else (if-let [f (get @cmd-args-registry op)]
                 (f cmd)
                 #js [])))))

;; LibraryContext: per library, ctx-id -> worker-idx, the CURL* and sqlite3*
;; affinity idiom. claim() picks the least-loaded worker and increments
;; claim_count in one step, so no dispatch can race the pick.

#?(:cljs
   (defn claim
     "Reserve the least-loaded worker through worker-router claim(). Returns
      the raw #js {index, release}. assign-worker-for-context! wraps it."
     [pool]
     (.claim pool)))

#?(:cljs
   (def ^:private DEFAULT-MIN-AGE-MS 100))

#?(:cljs
   (defn- now-ms [] (.now js/Date)))

;; Release Promises of wrapped disposers, for flush-pending-disposes!. They
;; live here, not in resource-tracker, to keep that close to tech.resource.

#?(:cljs
   (defonce ^:private pending-disposes (js/Set.)))

#?(:cljs
   (defonce ^:private pending-disposes-by-parent (atom {})))
;; parent-ctx-id -> #js [Promise ...], so a ctx destroy drains only its own
;; children.

#?(:cljs
   (defonce ^:private in-flight-by-parent (atom {})))
;; parent-ctx-id -> count of child handles registered with this parent whose
;; release has not settled. A live, undisposed child counts. A parent destroy
;; waits for zero. A membership poll is not enough: an entry leaves
;; live-handles before its worker_call posts.

#?(:cljs
   (defonce ^:private gate-promises-by-parent (atom {})))
;; parent-ctx-id -> {:promise :resolve}, resolved when in-flight reaches
;; zero. A parent destroy awaits it, because a setTimeout-0 poll starves
;; behind worker-queue traffic.

#?(:cljs
   (defn- ensure-gate-promise!
     "The deferred {:promise :resolve} for `parent`, created on first use."
     [parent]
     (when-not (contains? @gate-promises-by-parent parent)
       (let [resolve-fn (atom nil)
             promise (js/Promise. (fn [resolve _reject] (reset! resolve-fn resolve)))]
         (swap! gate-promises-by-parent
                (fn [m]
                  (if (contains? m parent)
                    m
                    (assoc m parent {:promise promise :resolve @resolve-fn}))))))
     (get @gate-promises-by-parent parent)))

#?(:cljs
   (defn- resolve-gate-promise!
     "Remove and resolve the gate Promise of `parent`, if it has one."
     [parent]
     (when-let [entry (get @gate-promises-by-parent parent)]
       (swap! gate-promises-by-parent dissoc parent)
       ((:resolve entry) nil))))

#?(:cljs
   (defn await-parent-drain!
     "A Promise that resolves when the in-flight count of `parent` is zero.
      Check the count again after it resolves, because a new
      register-handle! can raise it first."
     [parent]
     (if (zero? (get @in-flight-by-parent parent 0))
       (js/Promise.resolve nil)
       (:promise (ensure-gate-promise! parent)))))

#?(:cljs
   (defn- capture-pending-dispose!
     "When `result` is a Promise, add it to pending-disposes, and to the
      bucket of parent-ctx-id when that is not nil. Returns `result`."
     ([result] (capture-pending-dispose! result nil))
     ([result parent-ctx-id]
      (when (instance? js/Promise result)
        (.add pending-disposes result)
        ;; A fulfilled dispose leaves at once, since a page may flush only at
        ;; shutdown. A rejected one stays, so the flush reports it.
        (.then result (fn [_] (.delete pending-disposes result)) (fn [_] nil))
        (when (some? parent-ctx-id)
          (let [bucket (or (get @pending-disposes-by-parent parent-ctx-id)
                           (let [b #js []]
                             (swap! pending-disposes-by-parent assoc parent-ctx-id b)
                             b))]
            (.push bucket result))))
      result)))

#?(:cljs
   (defn drain-pending-disposes-for-parent!
     "Promise.allSettled over the dispose Promises captured under
      `parent-ctx-id` since its last drain. Clears the bucket first, so a
      concurrent capture goes to the next drain. Resolves at once for an
      empty or unknown bucket.

      A consumer's parent destroy (clj-proj's wasm/destroy-context!) awaits
      it before it posts context_destroy, so it cannot free the parent while
      a child release is in transit."
     [parent-ctx-id]
     (let [bucket (get @pending-disposes-by-parent parent-ctx-id)]
       (if (or (nil? bucket) (zero? (.-length bucket)))
         (js/Promise.resolve)
         (do
           (swap! pending-disposes-by-parent dissoc parent-ctx-id)
           (.allSettled js/Promise bucket))))))

#?(:cljs
   (defn flush-pending-disposes!
     "Promise.allSettled over the captured disposer Promises that are
      pending, or that rejected since the last flush. Clears the list first,
      so a concurrent dispose goes to the next flush. Await it before you
      terminate the workers, or an async destroy can fail to reach them."
     []
     (let [pending (.from js/Array pending-disposes)]
       (.clear pending-disposes)
       (.allSettled js/Promise pending))))

#?(:cljs
   (defn fire-and-capture-dispose!
     "Call `disposer-fn` and capture its Promise, so an explicit
      (Symbol.dispose) release drains through flush-pending-disposes! like a
      GC release. Traces `context-info` as EXPLICIT-DISPOSE."
     ([disposer-fn]
      (fire-and-capture-dispose! disposer-fn nil))
     ([disposer-fn context-info]
      (hrt/dbg "EXPLICIT-DISPOSE" (or context-info #js {}))
      (capture-pending-dispose! (disposer-fn)))))

#?(:cljs
   (defonce ^:private library-contexts (atom {})))
;; library-key ->
;;   {:ctx-workers (atom {ctx-id -> {:idx Integer :release Fn}})
;;    :live-handles    (atom {ctx-id -> {:owner WeakRef :release Fn
;;                                   :exec-unit Integer :parent-ctx-id
;;                                   :created-at ms :touched-at ms
;;                                   :refcount Integer}})
;;    :max-live-ctxs (Integer | nil)
;;    :min-age-ms   Integer
;;    :evicted (atom #{ctx-id ...})   ; LRU-reclaimed, tombstoned
;;    :stats (atom {:evictions Integer :blocks Integer})
;;    :worker-idx-extractor (Fn | nil)}
;;
;; ctx-workers holds claim releases. live-handles holds bounded-LRU entries.
;; A ctx-id can be in both.

#?(:cljs
   (defn- ensure-library!
     ([library-key] (ensure-library! library-key nil))
     ([library-key opts]
      (when-not (contains? @library-contexts library-key)
        (swap! library-contexts assoc library-key
               {:ctx-workers (atom {})
                :live-handles (atom {})
                :max-live-ctxs nil
                :min-age-ms DEFAULT-MIN-AGE-MS
                :evicted (atom #{})
                :stats (atom {:evictions 0 :blocks 0})}))
      (when (some? opts)
        (let [max-live-ctxs (opts-get opts :max-live-ctxs)
              min-age-ms (opts-get opts :min-age-ms)]
          (when (some? max-live-ctxs)
            (swap! library-contexts assoc-in [library-key :max-live-ctxs] max-live-ctxs))
          (when (some? min-age-ms)
            (swap! library-contexts assoc-in [library-key :min-age-ms] min-age-ms)))))))

#?(:cljs
   (defn register-library-context!
     "Set up the LibraryContext state of `library-key`. Idempotent.

      opts:
        :max-live-ctxs  The live-handle bound. nil means none.
        :min-age-ms     The eviction age gate. Default 100."
     ([library-key] (ensure-library! library-key))
     ([library-key opts] (ensure-library! library-key opts))))

#?(:cljs
   (defn- default-worker-idx-extractor
     "The worker index from an object's .worker_idx (munged :worker-idx) or
      a map's :worker-idx, else nil."
     [arg]
     (cond
       (and (object? arg) (some? (.-worker-idx arg)))
       (.-worker-idx arg)
       (and (map? arg) (:worker-idx arg))
       (:worker-idx arg)
       :else nil)))

#?(:cljs
   (defn register-worker-idx-predicate!
     "Set the worker-idx extractor of a library, a fn from one argument to
      an index or nil. Needed only when a handle keeps the index under
      another property name."
     [library-key extractor-fn]
     (ensure-library! library-key)
     (swap! library-contexts assoc-in [library-key :worker-idx-extractor] extractor-fn)))

#?(:cljs
   (defn worker-idx-from-args
     "The first worker-idx that the library extractor finds in `args`, else
      0. The 0 default pins unrouted calls to one worker, because each
      worker's wasm module holds its own state, such as MEMFS files and
      driver registries, and this layer cannot tell a pure call from one
      that touches it.

      To spread a known-pure call, call worker-call with a nil worker-idx,
      or pass :force-worker-idx through call!."
     [library-key args]
     (let [entry (get @library-contexts library-key)
           extract (or (:worker-idx-extractor entry) default-worker-idx-extractor)]
       (or (some extract args) 0))))

#?(:cljs
   (defn assign-worker-for-context!
     "Pick a worker for a new context. Returns {:idx :release}. An explicit
      :worker is bounds-checked and takes no claim. Without it,
      pool.claim() picks the least-loaded worker.

      Call release exactly once, on destroy or on create failure, so the
      claim_count of the pool drains."
     [pool library-key opts]
     (ensure-library! library-key)
     (if-let [explicit (:worker opts)]
       (let [worker-count (pool-size pool)]
         (when (>= explicit worker-count)
           (throw (js/Error. (str "Worker index " explicit " out of range (max " (dec worker-count) ")"))))
         {:idx explicit :release (fn [] nil)})
       (let [c (claim pool)]
         {:idx (.-index c) :release (.-release c)}))))

#?(:cljs
   (defn track-context!
     "Record ctx-id -> {:idx :release}, and track `owner` so release-fn
      fires when V8 collects it. release-fn must not close over `owner`, or
      the owner stays reachable. A CAS makes an explicit untrack plus a later
      GC fire one release."
     [library-key ctx-id worker-idx release-fn owner]
     (ensure-library! library-key)
     (let [ctx-workers (:ctx-workers (get @library-contexts library-key))
           fired? (atom false)
           wrapped (fn []
                     (if (compare-and-set! fired? false true)
                       (do
                         (hrt/dbg "FR-CALLBACK" #js {:lib (str library-key) :ctx-id ctx-id :kind "ctx" :worker worker-idx})
                         (swap! ctx-workers dissoc ctx-id)
                         (capture-pending-dispose! (release-fn)))
                       (hrt/dbg "FR-CALLBACK-SUPPRESSED" #js {:lib (str library-key) :ctx-id ctx-id :kind "ctx"})))]
       (resource/track owner #js {:tracktype "gc" :disposefn wrapped})
       (hrt/dbg "FR-TRACK" #js {:lib (str library-key) :ctx-id ctx-id :kind "ctx" :worker worker-idx})
       (swap! ctx-workers assoc ctx-id {:idx worker-idx :release wrapped}))))

#?(:cljs
   (defn untrack-context!
     "Fire the stored release of ctx-id, which drains its pool claim.
      Idempotent, and a no-op for an untracked ctx-id."
     [library-key ctx-id]
     (when-let [entry (get @library-contexts library-key)]
       (let [ctx-workers (:ctx-workers entry)
             stored (get @ctx-workers ctx-id)]
         (when-let [release (:release stored)]
           (release))))))

#?(:cljs
   (defn get-context-worker
     "The worker-idx of `ctx`, a map with :ctx-id or a raw ctx-id. 0 for an
      untracked ctx."
     [library-key ctx]
     (let [ctx-id (cond
                    (map? ctx) (:ctx-id ctx)
                    (number? ctx) ctx
                    :else ctx)]
       (if-let [entry (get @library-contexts library-key)]
         (:idx (get @(:ctx-workers entry) ctx-id) 0)
         0))))

#?(:cljs
   (defn reset-library-context!
     "Consumer shutdown. Fire every stored claim release and LRU disposer,
      then clear ctx-workers, live-handles and the eviction tombstones. The
      CAS on each release makes a later GC fire a no-op."
     [library-key]
     (when-let [entry (get @library-contexts library-key)]
       (let [ctx-workers (:ctx-workers entry)
             live-handles (:live-handles entry)]
         (doseq [[_ stored] @ctx-workers]
           (when-let [release (:release stored)]
             (release)))
         (reset! ctx-workers {})
         (when live-handles
           (doseq [[_ stored] @live-handles]
             (when-let [release (:release stored)]
               (release)))
           (reset! live-handles {}))
         (when-let [ev (:evicted entry)]
           (reset! ev #{}))))))

#?(:cljs
   (defn register-handle!
     "Track a handle in live-handles, and track `owner` so release-fn frees
      the native handle on GC. Returns ctx-id. release-fn must not close
      over `owner`, or the owner stays reachable. A CAS makes an eviction or
      manual release plus a later GC fire one release.

      With parent-ctx-id, the release Promise goes into that parent's
      bucket, and in-flight-by-parent counts the handle until the Promise
      settles. A parent destroy gates on that count."
     ([library-key ctx-id exec-unit-idx release-fn owner]
      (register-handle! library-key ctx-id exec-unit-idx release-fn owner nil))
     ([library-key ctx-id exec-unit-idx release-fn owner parent-ctx-id]
      (ensure-library! library-key)
      (let [lib (get @library-contexts library-key)
            live (:live-handles lib)
            fired? (atom false)
            decrement! (fn []
                         (when (some? parent-ctx-id)
                           (let [m (swap! in-flight-by-parent
                                          (fn [m]
                                            (let [c (get m parent-ctx-id 0)]
                                              (if (<= c 1)
                                                (dissoc m parent-ctx-id)
                                                (assoc m parent-ctx-id (dec c))))))]
                             (when (zero? (get m parent-ctx-id 0))
                               (resolve-gate-promise! parent-ctx-id)))))
            wrapped (fn []
                      (if (compare-and-set! fired? false true)
                        (do
                          (hrt/dbg "FR-CALLBACK" #js {:lib (str library-key) :ctx-id ctx-id :kind "handle" :worker exec-unit-idx})
                          (swap! live dissoc ctx-id)
                          (let [p (release-fn)
                                p* (if (instance? js/Promise p)
                                     (.finally p decrement!)
                                     (do (decrement!)
                                         p))]
                            (capture-pending-dispose! p* parent-ctx-id)))
                        (hrt/dbg "FR-CALLBACK-SUPPRESSED" #js {:lib (str library-key) :ctx-id ctx-id :kind "handle"})))]
        (resource/track owner #js {:tracktype "gc" :disposefn wrapped})
        (hrt/dbg "FR-TRACK" #js {:lib (str library-key) :ctx-id ctx-id :kind "handle" :worker exec-unit-idx})
        (swap! live assoc ctx-id
               {:owner (js/WeakRef. owner)
                :release wrapped
                :exec-unit exec-unit-idx
                :parent-ctx-id parent-ctx-id
                :created-at (now-ms)
                :touched-at (now-ms)
                :refcount 0})
        (swap! (:evicted lib) disj ctx-id)
        (when (some? parent-ctx-id)
          (swap! in-flight-by-parent
                 (fn [m] (assoc m parent-ctx-id (inc (get m parent-ctx-id 0))))))
        ctx-id))))

#?(:cljs
   (defn dispose-handle!
     "Fire the release of live handle ctx-id now, through the same wrapped
      disposer as GC, so the in-flight accounting runs. Returns release-fn's
      result (the dispose Promise), or undefined when the handle is gone and
      the caller must use its raw destroy-fn. release-fn must return a
      Promise, or the caller cannot tell the two apart. A raw destroy of a
      live handle never decrements in-flight-by-parent, and wedges a parent
      destroy."
     [library-key ctx-id]
     (when-let [lib (get @library-contexts library-key)]
       (when-let [stored (get @(:live-handles lib) ctx-id)]
         (when-let [release (:release stored)]
           (release))))))

#?(:cljs
   (defn in-flight-count-for-parent
     "The count of child handles registered under `parent` whose release has
      not settled, live children included."
     [parent]
     (get @in-flight-by-parent parent 0)))

#?(:cljs
   (defn ref-handle!
     "Increment the refcount of ctx-id and update :touched-at. Pair it with
      unref-handle! in a finally. Eviction skips an entry with a refcount
      above 0."
     [library-key ctx-id]
     (when-let [lib (get @library-contexts library-key)]
       (swap! (:live-handles lib)
              (fn [m]
                (if (contains? m ctx-id)
                  (-> m
                      (update-in [ctx-id :refcount] inc)
                      (assoc-in [ctx-id :touched-at] (now-ms)))
                  m))))))

#?(:cljs
   (defn unref-handle!
     "Decrement the refcount of ctx-id, with a floor of 0. A no-op for an
      untracked ctx-id."
     [library-key ctx-id]
     (when-let [lib (get @library-contexts library-key)]
       (swap! (:live-handles lib)
              (fn [m]
                (if (contains? m ctx-id)
                  ;; A count below zero would let the next ref-handle! leave
                  ;; an in-flight entry at 0, where eviction can free it
                  ;; mid-call.
                  (update-in m [ctx-id :refcount] (fn [rc] (max 0 (dec rc))))
                  m))))))

;; find-oldest-evictable and get-pool-detail share one eviction gate, so the
;; diagnostic matches the behavior. Each takes one `now` snapshot, so no
;; entry crosses the age boundary during a scan.

#?(:cljs
   (defn- entry-age-ms
     "The age of entry `e` at `now`, from :created-at, so a context that is
      touched often still ages out."
     [e now]
     (- now (or (:created-at e) (:touched-at e)))))

#?(:cljs
   (defn- entry-busy? [e] (pos? (:refcount e))))

#?(:cljs
   (defn- entry-evictable?
     [e now min-age-ms]
     (and (not (entry-busy? e))
          (>= (entry-age-ms e now) min-age-ms))))

#?(:cljs
   (defn- find-oldest-evictable
     "The least recently touched [ctx-id entry] that is idle and past
      min-age, or nil. Owner reachability does not count, so the cap
      reclaims idle contexts ahead of GC. The caller must
      invalidate-evicted! the entry, so a reuse fails cleanly."
     [live min-age-ms]
     (let [now (now-ms)
           evictable (filter (fn [kv] (entry-evictable? (val kv) now min-age-ms))
                             live)]
       (when (seq evictable)
         (apply min-key (fn [kv] (:touched-at (val kv))) evictable)))))

#?(:cljs
   (def ^:private evicted-owner-marker "__cljNativeEvicted"))

#?(:cljs
   (defn- invalidate-evicted!
     ;; Run before the release frees the native handle, so a reuse of the
     ;; still-reachable owner hits evicted? and not freed memory.
     [lib ctx-id entry]
     (when-let [owner (some-> ^js/WeakRef (:owner entry) (.deref))]
       (aset owner evicted-owner-marker true))
     (swap! (:evicted lib) conj ctx-id)))

#?(:cljs
   (defn evicted?
     "True when an LRU eviction reclaimed ctx-id and freed its native
      handle."
     [library-key ctx-id]
     (boolean (when-let [lib (get @library-contexts library-key)]
                (some-> (:evicted lib) deref (contains? ctx-id))))))

#?(:cljs
   (defn evict-oldest!
     "Invalidate and release the LRU evictable entry. Returns \"evicted\",
      \"none-evictable\" or \"empty\". Does not update :evictions, which
      counts only bounded-create-handle! evictions."
     [library-key]
     (when-let [lib (get @library-contexts library-key)]
       (let [live @(:live-handles lib)
             min-age (or (:min-age-ms lib) DEFAULT-MIN-AGE-MS)]
         (cond
           (empty? live) "empty"
           :else
           (if-let [[ctx-id entry] (find-oldest-evictable live min-age)]
             (do (invalidate-evicted! lib ctx-id entry)
                 ((:release entry))
                 "evicted")
             "none-evictable"))))))

#?(:cljs
   (defn bounded-create-handle!
     "Run `create-fn`, which must call register-handle!, under the
      max-live-ctxs bound, and return its result. At the bound, evict the
      LRU idle entry first. When every entry is busy or below min-age, throw
      ex-info with {:blocked :bounded-blocked}."
     [library-key create-fn]
     (ensure-library! library-key)
     (let [lib (get @library-contexts library-key)
           bound (:max-live-ctxs lib)]
       (when (and bound (>= (count @(:live-handles lib)) bound))
         (let [live @(:live-handles lib)
               min-age (or (:min-age-ms lib) DEFAULT-MIN-AGE-MS)]
           (if-let [[ctx-id entry] (find-oldest-evictable live min-age)]
             (do (invalidate-evicted! lib ctx-id entry)
                 ((:release entry))
                 (swap! (:stats lib) update :evictions inc))
             (do (swap! (:stats lib) update :blocks inc)
                 (throw (ex-info "live-handles at bound, no evictable entry"
                                 {:library library-key
                                  :live (count live)
                                  :max bound
                                  :blocked :bounded-blocked}))))))
       (create-fn))))

#?(:cljs
   (defn get-pool-detail
     "Diagnostic. Why each live entry is or is not evictable, as #js {total,
      evictable, blocked_refcount, blocked_age_gate, blocked_weakref (always
      0: owner reachability does not gate eviction), sample}. `sample` holds up to 8 blocked entries as {ctx_id, refcount,
      owner_alive, age_ms, age_gated}. undefined for an unregistered
      library."
     [library-key]
     (when-let [lib (get @library-contexts library-key)]
       (let [live @(:live-handles lib)
             min-age (or (:min-age-ms lib) DEFAULT-MIN-AGE-MS)
             now (now-ms)
             classify (fn [[id e]]
                        (let [owner (:owner e)]
                          {:id id
                           :refcount (:refcount e)
                           :owner-alive (boolean (and owner (.deref owner)))
                           :age-ms (entry-age-ms e now)
                           :age-gated (< (entry-age-ms e now) min-age)
                           :busy (entry-busy? e)
                           :evictable (entry-evictable? e now min-age)}))
             classified (mapv classify live)
             evictable (filterv :evictable classified)
             blocked (filterv (complement :evictable) classified)
             blk-ref (filterv :busy blocked)
             blk-age (filterv #(and (not (:busy %)) (:age-gated %)) blocked)
             blk-weak (filterv #(and (not (:busy %))
                                     (not (:age-gated %))
                                     (:owner-alive %)) blocked)
             ->js (fn [m]
                    #js {:ctx_id (str (:id m))
                         :refcount (:refcount m)
                         :owner_alive (:owner-alive m)
                         :age_ms (:age-ms m)
                         :age_gated (:age-gated m)})]
         #js {:total (count classified)
              :evictable (count evictable)
              :blocked_refcount (count blk-ref)
              :blocked_age_gate (count blk-age)
              :blocked_weakref (count blk-weak)
              :sample (mapv ->js (take 8 blocked))}))))

#?(:cljs
   (defn get-pool-stats
     "The counters of a library as #js {live, evictions, blocks,
      max_live_ctxs, min_age_ms}. undefined for an unregistered library."
     [library-key]
     (when-let [lib (get @library-contexts library-key)]
       (let [s @(:stats lib)]
         #js {:live (count @(:live-handles lib))
              :evictions (:evictions s)
              :blocks (:blocks s)
              :max_live_ctxs (:max-live-ctxs lib)
              :min_age_ms (:min-age-ms lib)}))))

;; These let a .cljc consumer compile on the JVM. Each throws at the call,
;; with its own name.
#?(:clj
   (do
     (defn- unsupported
       [fn-name]
       (throw (ex-info "JVM uses GraalVM, not the worker pool" {:fn fn-name})))
     (defn broadcast-to-handlers! [_pool _handler-keys _method-name _args-fn]
       (unsupported 'broadcast-to-handlers!))
     (defn init-pool! [_opts] (unsupported 'init-pool!))
     (defn worker-call [_pool _handler-key _method-name _args _worker-idx]
       (unsupported 'worker-call))
     (defn terminate-pool! [_pool] (unsupported 'terminate-pool!))
     (defn pool-size [_pool] (unsupported 'pool-size))
     (defn register-cmd-args! [_op-name _f] (unsupported 'register-cmd-args!))
     (defn cmd-args [_cmd] (unsupported 'cmd-args))
     (defn register-library-context!
       ([_library-key] (unsupported 'register-library-context!))
       ([_library-key _opts] (unsupported 'register-library-context!)))
     (defn assign-worker-for-context! [_pool _library-key _opts]
       (unsupported 'assign-worker-for-context!))
     (defn claim [_pool] (unsupported 'claim))
     (defn await-parent-drain! [_parent] (unsupported 'await-parent-drain!))
     (defn drain-pending-disposes-for-parent! [_parent-ctx-id]
       (unsupported 'drain-pending-disposes-for-parent!))
     (defn flush-pending-disposes! [] (unsupported 'flush-pending-disposes!))
     (defn fire-and-capture-dispose!
       ([_disposer-fn] (unsupported 'fire-and-capture-dispose!))
       ([_disposer-fn _context-info] (unsupported 'fire-and-capture-dispose!)))
     (defn track-context! [_library-key _ctx-id _worker-idx _release-fn _owner]
       (unsupported 'track-context!))
     (defn untrack-context! [_library-key _ctx-id] (unsupported 'untrack-context!))
     (defn get-context-worker [_library-key _ctx] (unsupported 'get-context-worker))
     (defn reset-library-context! [_library-key] (unsupported 'reset-library-context!))
     (defn register-worker-idx-predicate! [_library-key _extractor-fn]
       (unsupported 'register-worker-idx-predicate!))
     (defn worker-idx-from-args [_library-key _args] (unsupported 'worker-idx-from-args))
     (defn register-handle!
       ([_library-key _ctx-id _exec-unit-idx _release-fn _owner]
        (unsupported 'register-handle!))
       ([_library-key _ctx-id _exec-unit-idx _release-fn _owner _parent-ctx-id]
        (unsupported 'register-handle!)))
     (defn dispose-handle! [_library-key _ctx-id] (unsupported 'dispose-handle!))
     (defn in-flight-count-for-parent [_parent]
       (unsupported 'in-flight-count-for-parent))
     (defn ref-handle! [_library-key _ctx-id] (unsupported 'ref-handle!))
     (defn unref-handle! [_library-key _ctx-id] (unsupported 'unref-handle!))
     (defn evict-oldest! [_library-key] (unsupported 'evict-oldest!))
     (defn bounded-create-handle! [_library-key _create-fn]
       (unsupported 'bounded-create-handle!))
     (defn get-pool-detail [_library-key] (unsupported 'get-pool-detail))
     (defn get-pool-stats [_library-key] (unsupported 'get-pool-stats))
     (defn evicted? [_library-key _ctx-id] (unsupported 'evicted?))
     ;; A no-op, so a consumer needs no platform branch for a JS-only
     ;; diagnostic.
     (defn set-log-config! [_opts] nil)))
