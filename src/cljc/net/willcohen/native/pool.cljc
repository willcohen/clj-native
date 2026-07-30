;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#?(:clj
   (ns net.willcohen.native.pool
     "JVM stub. The JVM path uses GraalVM Polyglot, and not the JS worker
      pool. Every fn here throws, thus incorrect use fails loudly and does
      not degrade silently. set-log-config! is the one exception, and it
      carries its own note below. pool-stub-test pins this block against the
      CLJS surface, name for name and arity for arity.

      The CLJS branch holds the real implementation, a Clojure wrapper
      around the worker-router substrate.")
   :cljs
   (ns net.willcohen.native.pool
     "Clojure wrapper around worker-router/WorkerPool. It supplies
      init-pool!, worker-call, terminate-pool! and the accessors.

      A caller brings its own workers. init-pool! adopts a supplied :pool
      and sets owned? to false. Without one, it spawns a fresh pool from the
      handler map of the caller, and sets owned? to true.

      The context lifetime of each library delegates to resource-tracker,
      the JS port of tech.resource. It tracks a JS owner in gc mode. The
      disposefn of that owner fires from the FinalizationRegistry when V8
      collects it.

      The bounded-LRU helpers enforce max-live-ctxs. They evict an idle
      context directly, ahead of GC. Those helpers are register-handle!,
      ref-handle!, unref-handle!, evict-oldest! and
      bounded-create-handle!."
     (:require ["worker-router" :as cp]
               ["resource-tracker" :as resource]
               ["./handler_runtime.mjs" :as hrt]
               [clojure.string :as string])))

#?(:cljs
   (defn- handler-spec->js
     [spec]
     ;; Squint compiles a map literal to a JS-native shape. Thus clj->js does
     ;; nothing, and a Clojure-style map reads the same as the plain JS
     ;; object of a TypeScript caller. Only :module and :init reach the
     ;; worker.
     (let [obj (js-obj)]
       (when-let [m (:module spec)]
         (aset obj "module" m))
       (when (contains? spec :init)
         (aset obj "init" (:init spec)))
       obj)))

#?(:cljs
   (defn- handlers->js
     [handlers]
     ;; The local is named put!, and not assoc!. squint 0.14.202 and later
     ;; special-case the assoc! symbol at every call site. They arity-check
     ;; it against the built-in shape even when a local binding shadows it,
     ;; and the compile then fails.
     ;;
     ;; One walk covers the two shapes. A squint map and the plain JS object
     ;; of a caller are each plain objects. Thus doseq gives the same pairs,
     ;; and a reverse-DNS string key such as "com.example.mylib" survives in
     ;; each case.
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
     "Read k from a Clojure map, or read its snake_case name from a JS
      object. A squint map compiles to a plain JS object, thus (map? #js {})
      is also true. The JS branch MUST come before the map branch. Otherwise
      (get js-obj :a-b) returns nil silently for every kebab-case key."
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
     ;; squint-cljs/core has no keyword?. Discriminate with string?, and use
     ;; name for the other case, which works on a keyword.
     [x]
     (if (string? x) x (name x))))

#?(:cljs
   (defn ^:async broadcast-to-handlers!
     "Call `method-name` on every (worker x handler-key) cell of the pool.
      `args-fn` builds the arguments for each worker index.

      This fn reaches every handler key, and not the first key only. A joint
      pool can inline handler_runtime one time for each bundle, and each copy
      carries its own logState. Thus a call to one key leaves the rest
      unconfigured, and nothing reports that.

      Each call has its own guard. A handler with a bundle against an older
      clj-native has no such method, and that must not fail pool init."
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
     "Adopt a pool from the caller, or spawn a fresh pool. Returns {pool,
      owned?}. owned? is true if and only if this call constructed the pool.

      opts:
        :pool       Adopt this WorkerPool, and do no spawn.
        :handlers   handler-key -> {:module url :init args}. Each key is a
                    keyword or a string. This key is necessary when :pool is
                    absent.
        :size       :auto, an integer, or \"auto\". The default is :auto.
        :bootstrap  The bootstrap URL. The default resolves
                    worker-router/worker-bootstrap.
        :handler-runtime  An optional diagnostic config. init-pool!
                    broadcasts it to every worker through __setLogConfig. It
                    ignores this key on adoption."
     [opts]
     (let [caller-pool (opts-get opts :pool)]
       (if (some? caller-pool)
         ;; The JS property name is `owned`, with no `?`. That prevents an
         ;; asymmetry in the squint keyword munging. `#js {:owned? ...}`
         ;; writes the literal property name "owned?". But
         ;; `(.-owned? result)` reads the munged `.owned_QMARK_`. Thus the
         ;; two sides never see the same key.
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
           ;; Broadcast inline, and not through worker-call. worker-call
           ;; would be a forward reference, and it would add queue trace
           ;; noise.
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
           ;; Always send the slot index of each worker, even with the
           ;; diagnostic substrate off.
           (await (broadcast-to-handlers! pool handler-keys "__setWorkerSlot"
                                          (fn worker-slot-args [w] #js [w])))
           #js {:pool pool :owned true})))))

#?(:cljs
   ;; Pool-wide monotonic dispatch counter. Every QUEUE-DISPATCH and
   ;; QUEUE-COMPLETE carries it as `disp-id`. Thus a tail-grep can pair a
   ;; start event with an end event, even across reordered worker output.
   ;; This counter differs from the handler-runtime `id` field. That field
   ;; counts the BUSY and DESTROY calls of each handler, and not the
   ;; dispatches of each pool.
   (defonce ^:private dispatch-id-counter (atom 0)))

#?(:cljs
   (defn- sanitize-reason
     "Munge an error message for an inline trace. Collapse each whitespace
      run and each `=` character to `_`, so the formatEvent key=value parser
      stays correct. Cap the result at 200 characters."
     [msg]
     (let [s (if (string? msg) msg (str msg))
           one-line (string/replace s #"[\s=]+" "_")]
       (if (> (count one-line) 200) (subs one-line 0 200) one-line))))

#?(:cljs
   (defn ^:async worker-call
     "Dispatch a method on a registered handler. Returns the awaited result
      of the call.

      With a nil worker-idx, this fn routes through pool.any(), which picks
      the least-loaded worker. With an integer worker-idx, 0 included, it
      routes through pool.worker(idx) for affinity.

      `args` is a JS array, or nil. The native `Function.prototype.apply` of
      the host spreads it into the proxied method. The squint `apply` would
      coerce a `#js []` through the cljs.core seq machinery, which can throw
      `Cannot convert object to primitive value` on a Comlink proxy.

      This is the raw dispatch primitive, and it does NOT refcount. call!
      puts ref-handle! and unref-handle! around each ccall. The signature
      here carries no ctx-id. Do not fold the refcount in, because a
      lifecycle op bypasses it for a good reason."
     [pool handler-key method-name args worker-idx]
     (let [target        (if (some? worker-idx)
                           (.worker pool worker-idx)
                           (.any pool))
           handler-name  (str handler-key)
           handler-proxy (aget target handler-name)
           method-fn     (aget handler-proxy method-name)
           call-args     (or args #js [])
           worker-tag    (if (some? worker-idx) worker-idx "auto")
           ;; call-args[0] is the ccall C-fn name. It is nil for an op that
           ;; is not a ccall.
           c-fn          (when (= method-name "ccall") (aget call-args 0))
           ;; Read the enablement one time, so a paired open and close match
           ;; even when setLogConfig changes the level during the call. Skip
           ;; the counter when the substrate is off.
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
     "Calls pool.terminate(). This fn is internal to the registry. The
      shutdown-pool! of the workload-pool registry is the consumer-facing
      teardown, and it already skips an adopted pool that the caller owns. A
      consumer does not call this fn directly."
     [pool]
     (await (.terminate pool))))

#?(:cljs
   (defn pool-size
     "The integer worker count of the pool."
     [pool]
     (.-size pool)))

;; Diagnostic substrate config. The host side uses set-log-config!. The
;; worker side uses the init-args {:handlerRuntime {:logLevel
;; :logCategories}}. It is opt-in, and off by default.

#?(:cljs
   (defn set-log-config!
     "Configure the diagnostic substrate on the host side.

      opts:
        :level      :off, :error, :warn, :info, :debug or :trace. A nil
                    value means off. Every current event is debug-grade.
        :categories The keywords or strings to allow. A nil value allows
                    all. A category is the lower-case event-tag prefix, thus
                    BUSY-* is :busy and FR-* is :fr.

      An absent key leaves that state unchanged. A nil opts disables the
      substrate. For example:
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

;; Cmd-envelope translator, from {:cmd ...} to JS positional args. The
;; generic ccall is built in. A library-specific op registers its translator
;; through register-cmd-args!.

#?(:cljs
   (defonce ^:private cmd-args-registry (atom {})))

#?(:cljs
   (defn register-cmd-args!
     "Register a translator for a custom op. The translator maps a cmd-map
      to the #js positional args. Thus worker-call stays shape-agnostic for
      a library-specific op, such as context_create or context_destroy."
     [op-name f]
     (swap! cmd-args-registry assoc op-name f)))

#?(:cljs
   (defn- ccall-args
     "Build the 5-argument JS array for a ccall envelope. A non-standard key
      travels in the trailing `extra` object, and a consumer handler reads it
      there."
     [cmd]
     (let [extra    (js-obj)
           std-keys #{:cmd :fn :returnType :argTypes :args}]
       (doseq [[k v] cmd]
         (when (and (some? v) (not (contains? std-keys k)))
           (aset extra (str k) v)))
       #js [(:fn cmd) (:returnType cmd) (:argTypes cmd) (:args cmd) extra])))

#?(:cljs
   (defn cmd-args
     "Translate a {:cmd ...} envelope to the #js positional args of the
      handler method. \"ccall\" is built in. Another op uses its
      register-cmd-args! translator. An unknown op gives an empty arg list."
     [cmd]
     (let [op (:cmd cmd)]
       (cond
         (= op "ccall") (ccall-args cmd)
         :else (if-let [f (get @cmd-args-registry op)]
                 (f cmd)
                 #js [])))))

;; LibraryContext. For each library, it tracks ctx-id -> worker-idx. This is
;; the CURL* and sqlite3* threading idiom. The placement uses the
;; worker-router claim(). claim() picks the least-loaded worker, and it
;; increments claim_count atomically with that pick. Thus there is no race
;; between the pick and the dispatch. Each ctx carries its own release fn,
;; and a destroy fires that fn.

#?(:cljs
   (defn claim
     "Reserve a worker through the worker-router claim(). Returns the JS
      {index, release} object. clj-native wraps it in
      assign-worker-for-context!. A direct consumer can use this fn when it
      must have the raw handle."
     [pool]
     (.claim pool)))

#?(:cljs
   (def ^:private DEFAULT-MIN-AGE-MS 100))

#?(:cljs
   (defn- now-ms [] (.now js/Date)))

;; A wrapped disposer captures its release Promise here. Thus a consumer can
;; call flush-pending-disposes! before it terminates the pool. Without that
;; flush, an async destroy can fail to reach the worker. This does not live
;; in resource-tracker, which stays close to tech.resource. The async flush
;; is a clj-native concern.

#?(:cljs
   (defonce ^:private pending-disposes (atom #js [])))

#?(:cljs
   (defonce ^:private pending-disposes-by-parent (atom {})))
;; parent-ctx-id -> #js [Promise ...]. This lets a ctx destroy drain ITS
;; children only, and not every in-flight dispose. The 2-arity form of
;; capture-pending-dispose! populates it. A nil parent goes to the flat list
;; only.

#?(:cljs
   (defonce ^:private in-flight-by-parent (atom {})))
;; parent-ctx-id -> the count of child handles with an unsettled dispose
;; worker_call. The 6-arity register-handle! increments it. It decrements
;; when the release Promise settles. destroy-context! gates on a value of
;; zero. That closes the TOCTOU window, where a membership poll saw the entry
;; dissoc'd before its worker_call posted.

#?(:cljs
   (defonce ^:private gate-promises-by-parent (atom {})))
;; parent-ctx-id -> {:promise :resolve}. destroy-context! awaits this, and it
;; does no poll. The promise resolves when in-flight reaches zero. A
;; setTimeout-0 poll here starves behind the worker-queue traffic.

#?(:cljs
   (defn- ensure-gate-promise!
     "The deferred {:promise :resolve} for `parent`. This fn creates it
      lazily. Single-threaded JS makes the create-if-missing free of a
      race."
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
     "When `parent` has a deferred gate Promise, remove the entry and
      resolve the Promise with nil. The decrement! of the 6-arity
      register-handle! calls this fn when in-flight-by-parent[parent] reaches
      zero."
     [parent]
     (when-let [entry (get @gate-promises-by-parent parent)]
       (swap! gate-promises-by-parent dissoc parent)
       ((:resolve entry) nil))))

#?(:cljs
   (defn await-parent-drain!
     "A Promise that resolves when the in-flight count of the parent reaches
      zero. It resolves at once when that count is zero already. The caller
      MUST check again after the resolve. A new register-handle! can
      increment the count between the resolve and the next step of the
      caller."
     [parent]
     (if (zero? (get @in-flight-by-parent parent 0))
       (js/Promise.resolve nil)
       (:promise (ensure-gate-promise! parent)))))

#?(:cljs
   (defn- capture-pending-dispose!
     "When `result` is a Promise, push it onto pending-disposes. When
      parent-ctx-id is not nil, also push it onto the bucket for that parent
      in pending-disposes-by-parent. Thus destroy-context! can drain the
      children of this ctx only. Returns `result` unchanged, so a caller
      chain sees the original value."
     ([result] (capture-pending-dispose! result nil))
     ([result parent-ctx-id]
      (when (instance? js/Promise result)
        (.push @pending-disposes result)
        (when (some? parent-ctx-id)
          (let [bucket (or (get @pending-disposes-by-parent parent-ctx-id)
                           (let [b #js []]
                             (swap! pending-disposes-by-parent assoc parent-ctx-id b)
                             b))]
            (.push bucket result))))
      result)))

#?(:cljs
   (defn drain-pending-disposes-for-parent!
     "Return a Promise.allSettled over the handle dispose Promises under
      `parent-ctx-id`. It covers the Promises captured since the last drain
      of that bucket. This fn clears the bucket before it awaits. Thus a
      concurrent capture lands on the next drain, and nothing is lost. It
      resolves immediately when the bucket is empty or unknown, so a caller
      can always await it.

      Pair this fn with destroy-context!. Then the ctx destroy awaits the
      already-firing call!s of its children, before it posts context_destroy
      to the worker. Without this gate, the destroy can race ahead of an
      in-flight handle dispose. It then frees the parent context while the
      child release is still in transit."
     [parent-ctx-id]
     (let [bucket (get @pending-disposes-by-parent parent-ctx-id)]
       (if (or (nil? bucket) (zero? (.-length bucket)))
         (js/Promise.resolve)
         (do
           (swap! pending-disposes-by-parent dissoc parent-ctx-id)
           (.allSettled js/Promise bucket))))))

#?(:cljs
   (defn flush-pending-disposes!
     "A Promise.allSettled over the disposer Promises captured since the last
      flush. This fn clears the list before it awaits, so a concurrent
      dispose lands on the next flush. Await this fn before you terminate the
      workers. Otherwise an async destroy can fail to reach them."
     []
     (let [pending @pending-disposes]
       (reset! pending-disposes #js [])
       (.allSettled js/Promise pending))))

#?(:cljs
   (defn fire-and-capture-dispose!
     "Fire `disposer-fn`, and capture its Promise into pending-disposes.
      Thus an explicit (Symbol.dispose) release drains through
      flush-pending-disposes!, the same as a GC-fired release. This fn emits
      the optional context-info as EXPLICIT-DISPOSE."
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
;;                                   :exec-unit Integer :touched-at ms
;;                                   :refcount Integer}})
;;    :max-live-ctxs (Integer | nil)
;;    :min-age-ms   Integer
;;    :evicted (atom #{ctx-id ...})   ; LRU-reclaimed, tombstoned
;;    :stats (atom {:evictions Integer :blocks Integer})
;;    :worker-idx-extractor (Fn | nil)}
;;
;; ctx-workers holds the claim drainers. live-handles holds the bounded-LRU
;; entries. The two can overlap on the same ctx-id, or they can run in
;; parallel for each library.

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
                ;; The ctx-ids that an eviction reclaimed, as tombstones.
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
     "Declare that library-key uses the LibraryContext pattern, and
      initialize the state for that library. This fn is idempotent.

      opts:
        :max-live-ctxs The bound. A nil value means no bound.
        :min-age-ms    The eviction age gate. The default is 100."
     ([library-key] (ensure-library! library-key))
     ([library-key opts] (ensure-library! library-key opts))))

#?(:cljs
   (defn- default-worker-idx-extractor
     "The default convention. An object with .worker_idx, the munged form of
      :worker-idx, carries the worker index. A map with :worker-idx carries
      it too."
     [arg]
     (cond
       (and (object? arg) (some? (.-worker-idx arg)))
       (.-worker-idx arg)
       (and (map? arg) (:worker-idx arg))
       (:worker-idx arg)
       :else nil)))

#?(:cljs
   (defn register-worker-idx-predicate!
     "Override the default worker-idx extractor for a library. The extractor
      is a fn from an argument to an index, or to nil. This is necessary only
      when a handle carries the index under a non-standard property name.
      worker-idx-from-args reads the registry slot that this fn writes, on
      every affinity-routed call."
     [library-key extractor-fn]
     (ensure-library! library-key)
     (swap! library-contexts assoc-in [library-key :worker-idx-extractor] extractor-fn)))

#?(:cljs
   (defn worker-idx-from-args
     "Affinity routing. Scan `args` with the extractor of the library, and
      return the first worker-idx that an argument carries. Return 0 when no
      argument carries one.

      The 0 result pins every call with no affinity to worker 0, on purpose.
      A wasm module for each worker is worker-local state. That state covers
      MEMFS files, driver registries, error state and the allocator. This
      layer cannot separate a pure call from a call that creates or reads
      that state. Thus an unrouted call gets a deterministic worker, and not
      a least-loaded one.

      A consumer that knows a call is spreadable routes that call itself. It
      calls worker-call directly with a nil worker-idx for the any() path, or
      it passes :force-worker-idx through call!."
     [library-key args]
     (let [entry (get @library-contexts library-key)
           extract (or (:worker-idx-extractor entry) default-worker-idx-extractor)]
       (or (some extract args) 0))))

#?(:cljs
   (defn assign-worker-for-context!
     "Pick a worker for a new context. Returns {:idx :release}. This fn
      bounds-checks an explicit :worker, and it takes no claim for that case.
      Without :worker, pool.claim() picks the least-loaded worker.

      A consumer MUST fire release exactly one time, on a destroy or on a
      create failure. Then the claim_count of the pool drains."
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
     "Record ctx-id -> {:idx :release}. Also register `owner` for GC
      reclaim, so release-fn fires when V8 collects the owner.

      release-fn MUST NOT close over `owner`, because that would pin the
      owner. A CAS wraps the disposer. Thus an explicit untrack and a later
      FinalizationRegistry fire stay one release."
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
     "Fire the stored release for ctx-id, which drains the claim of the pool.
      This fn does nothing for an untracked ctx-id. The wrapped-release CAS
      makes it idempotent."
     [library-key ctx-id]
     (when-let [entry (get @library-contexts library-key)]
       (let [ctx-workers (:ctx-workers entry)
             stored (get @ctx-workers ctx-id)]
         (when-let [release (:release stored)]
           (release))))))

#?(:cljs
   (defn get-context-worker
     "The worker-idx for a ctx. The ctx is a map with :ctx-id, or a raw
      ctx-id. Returns 0 for an untracked ctx, which keeps backward
      compatibility."
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
     "Fire every stored release, which covers the claims and the LRU
      disposers. Then clear ctx-workers, live-handles and the eviction
      tombstones. This is the consumer shutdown path. The CAS on each release
      makes a later FinalizationRegistry fire do nothing."
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
     "Track a handle in live-handles. Also register `owner` with
      resource-tracker, so release-fn fires on GC. Returns ctx-id.

      release-fn releases the native handle. It MUST NOT close over `owner`,
      because that would pin the owner. A CAS guard makes this fn idempotent.
      Thus an eviction or a manual release, with a later FinalizationRegistry
      fire, stays one release.

      The 6-arity form attributes the handle to parent-ctx-id. It buckets the
      release Promise for destroy-context! to await. It also increments
      in-flight-by-parent here, and decrements it on settle. That pair is the
      TOCTOU gate. The 5-arity form records no parent."
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
        ;; A fresh handle removes the tombstone of a recurring ctx-id.
        (swap! (:evicted lib) disj ctx-id)
        (when (some? parent-ctx-id)
          (swap! in-flight-by-parent
                 (fn [m] (assoc m parent-ctx-id (inc (get m parent-ctx-id 0))))))
        ctx-id))))

#?(:cljs
   (defn dispose-handle!
     "Fire the release of a live handle at once, by ctx-id. It uses the SAME
      wrapped disposer that GC uses. Thus an explicit (Symbol.dispose)
      release runs the full accounting: the CAS guard, the dissoc, the
      in-flight decrement, and the capture.

      Returns the dispose Promise. Returns nil when the handle is gone, and
      the caller must then use its raw destroy-fn. A raw destroy that skips
      this fn never decrements in-flight-by-parent, and that wedges the drain
      gate of destroy-context!."
     [library-key ctx-id]
     (when-let [lib (get @library-contexts library-key)]
       (when-let [stored (get @(:live-handles lib) ctx-id)]
         (when-let [release (:release stored)]
           (release))))))

#?(:cljs
   (defn in-flight-count-for-parent
     "The count of child handles of `parent` with an unsettled dispose
      worker_call. destroy-context! gates on a value of zero."
     [parent]
     (get @in-flight-by-parent parent 0)))

#?(:cljs
   (defn ref-handle!
     "Increment the refcount, and update :touched-at. Dispatch calls this fn
      on entry, for any ccall with a reference to ctx-id. It pairs with
      unref-handle! in the dispatch finally. The eviction guard uses a
      refcount above 0 to skip an in-flight context."
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
     "Decrement the refcount. Dispatch calls this fn on exit, in the finally,
      and that includes the throw path. It does nothing for an untracked
      ctx-id."
     [library-key ctx-id]
     (when-let [lib (get @library-contexts library-key)]
       (swap! (:live-handles lib)
              (fn [m]
                (if (contains? m ctx-id)
                  ;; Keep 0 as the floor. An unpaired or double unref must not
                  ;; drive the refcount below zero. Otherwise the (zero?
                  ;; refcount) test of the eviction guard never fires, and the
                  ;; entry pins forever.
                  (update-in m [ctx-id :refcount] (fn [rc] (max 0 (dec rc))))
                  m))))))

;; The eviction gate, stated one time. find-oldest-evictable picks by it, and
;; get-pool-detail reports on it. Thus the diagnostic cannot drift from the
;; behavior that it explains. Each one takes a single `now` snapshot and
;; passes it down. Thus no entry can cross the age boundary during a scan.

#?(:cljs
   (defn- entry-age-ms
     "The age of a live-handles entry, against the `now` snapshot of the
      caller. This fn reads :created-at. Thus a context that updates
      :touched-at again and again still ages out."
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
     "The LRU [ctx-id entry] with a refcount of 0, above min-age. Returns nil
      when there is none. Owner reachability does not gate this choice. Thus
      the cap reclaims an idle context ahead of GC. The caller then calls
      invalidate-evicted! on it, so a reuse fails cleanly."
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
     ;; Tombstone the ctx-id, and mark the owner, which is still reachable.
     ;; Do this before anything frees the native handle. Thus a reuse reaches
     ;; evicted?, and it does not reach freed memory.
     [lib ctx-id entry]
     (when-let [owner (some-> ^js/WeakRef (:owner entry) (.deref))]
       (aset owner evicted-owner-marker true))
     (swap! (:evicted lib) conj ctx-id)))

#?(:cljs
   (defn evicted?
     "Returns true if and only if an LRU eviction reclaimed ctx-id. Such an
      eviction frees the native handle."
     [library-key ctx-id]
     (boolean (when-let [lib (get @library-contexts library-key)]
                (some-> (:evicted lib) deref (contains? ctx-id))))))

#?(:cljs
   (defn evict-oldest!
     "Reclaim the LRU evictable entry. This fires its release, and
      invalidates it. Returns \"evicted\", \"none-evictable\", or \"empty\".

      An evictable entry has a refcount of 0 and an age above min-age. Refer
      to find-oldest-evictable. This fn does not touch the stats counter.
      bounded-create-handle! owns that counter, which prevents a double
      count."
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
     "Run `create-fn` under the max-live-ctxs gate. `create-fn` must call
      register-handle!.

      At the bound or above it, this fn evicts the LRU idle entry first. When
      no entry is evictable, it throws an ex-info with
      {:blocked :bounded-blocked}. No entry is evictable when every entry is
      busy, with a refcount above 0, or is below min-age. Returns the result
      of `create-fn`."
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
     "DIAGNOSTIC. A breakdown for each entry, of why that entry is evictable
      or is not. Returns a plain JS object of {total, evictable,
      blocked_refcount, blocked_age_gate, blocked_weakref, sample}.

      `sample` lists 8 blocked entries at most. Each one carries the fields
      {ctx_id, refcount, owner_alive, age_ms, age_gated}. Returns undefined
      when the library is not registered."
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
     "Return the counters of each library as a plain JS object. Thus a JS
      caller and the LRU acceptance test get property-access semantics. The
      shape is {live, evictions, blocks, max_live_ctxs, min_age_ms}. Returns
      undefined when the library is not registered."
     [library-key]
     (when-let [lib (get @library-contexts library-key)]
       (let [s @(:stats lib)]
         #js {:live (count @(:live-handles lib))
              :evictions (:evictions s)
              :blocks (:blocks s)
              :max_live_ctxs (:max-live-ctxs lib)
              :min_age_ms (:min-age-ms lib)}))))

;; A .cljc consumer that requires this namespace cross-platform resolves
;; these on the JVM. Each one fails loudly at the call, with the correct
;; name. That is better than a break of the consumer compile on an unresolved
;; var.
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
     ;; The diagnostic substrate is JS-only at this time. This JVM version
     ;; does nothing, thus a consumer can call it with no platform branch.
     (defn set-log-config! [_opts] nil)))
