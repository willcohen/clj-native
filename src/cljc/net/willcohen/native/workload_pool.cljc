;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#?(:clj
   (ns net.willcohen.native.workload-pool
     "Worker pools shared by the libraries that call register-handler!.

      JVM: three ExecutorService slots, :mixed, :io and :compute, for the
      core.async.flow :mixed-exec, :io-exec and :compute-exec. Each thread
      runs the handler inits of its slot once, before its first task, and
      keeps the state in a thread-local that current-context reads."
     (:require [clojure.tools.logging :as log])
     (:import [java.util.concurrent
               ExecutorService
               LinkedBlockingQueue
               ThreadFactory
               ThreadPoolExecutor
               TimeUnit]))
   :cljs
   (ns net.willcohen.native.workload-pool
     "Worker pools: one worker-router pool shared by every library that
      calls register-handler!, so a cross-library pipeline stays on one
      worker. The workload argument is ignored, because JS work holds a
      thread only while it computes.

      A consumer that builds its registry during init uses a wiring
      (make-wiring!, ensure-wired!), because a registry cannot guard the
      setup that creates it."
     (:require ["./pool.mjs" :as pool])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (def ^:private ^ThreadLocal thread-handler-state
     ;; {lib-key -> handler-state}. Empty on a non-pool thread, which is how
     ;; current-context detects one.
     (proxy [ThreadLocal] []
       (initialValue [] {}))))

#?(:clj
   (def ^:private ^ThreadLocal thread-init-errors
     ;; {lib-key -> Throwable} of the inits that failed on this thread.
     (proxy [ThreadLocal] []
       (initialValue [] {}))))

#?(:clj
   (defn- run-handler-inits!
     "Run each handler init that has not succeeded on this thread. It never
      throws: a failed init is kept for current-context to throw, and runs
      again before the next task."
     [handlers]
     (doseq [{:keys [lib-key init args]} handlers
             :when (not (contains? (.get thread-handler-state) lib-key))]
       (try
         (.set thread-handler-state
               (assoc (.get thread-handler-state) lib-key (when init (init args))))
         (.set thread-init-errors (dissoc (.get thread-init-errors) lib-key))
         (catch Throwable e
           (.set thread-init-errors (assoc (.get thread-init-errors) lib-key e)))))))

#?(:clj
   (defn- run-handler-destroys!
     "Run each handler destroy on the state of this thread, in reverse
      registration order. Then clear the ThreadLocal."
     [handlers]
     (let [state (.get thread-handler-state)]
       (doseq [{:keys [lib-key destroy]} (reverse handlers)]
         (when (and destroy (contains? state lib-key))
           (try
             (destroy (get state lib-key))
             (catch Throwable e
               ;; One bad destroy must not block the others.
               (log/error
                e "clj-native: destroy failed for lib" lib-key)))))
       (.set thread-handler-state {}))))

;; The explicit release and the GC dispose-fn call the same native destructor.
;; A CAS on a weak-keyed flag for each pointer stops the double free.

#?(:clj
   (def ^:private ^java.util.Map released-flags
     (java.util.Collections/synchronizedMap (java.util.WeakHashMap.))))

#?(:clj
   (defn release-once!
     "Run zero-arg `release-fn` at most once for each `pointer`, across
      threads, while the first `pointer` object passed stays reachable (the
      flags have weak keys). Pass a dt-ffi Pointer or a pointer record,
      not a long: a long boxes anew on each call, so after a GC release-fn
      can run again. Returns true when it ran here, else nil.

      With a non-nil `lock`, release-fn also runs under that lock. Pass one
      lock for each library with process-global state, where the destroys of
      two different pointers can race into a SIGSEGV."
     [pointer lock release-fn]
     (when pointer
       (let [^java.util.concurrent.atomic.AtomicBoolean flag
             (.computeIfAbsent released-flags pointer
                               (reify java.util.function.Function
                                 (apply [_ _] (java.util.concurrent.atomic.AtomicBoolean. false))))]
         (when (.compareAndSet flag false true)
           (if lock
             (locking lock (release-fn))
             (release-fn))
           true)))))

#?(:clj
   (defn- make-thread-factory
     "A ThreadFactory of daemon threads named clj-native-<workload>-<n>. Each
      thread is in (:threads registry) until it has run the handler destroys
      of `workload` at its end."
     ^ThreadFactory [registry workload]
     (let [handlers (:handlers registry)
           threads  (:threads registry)
           counter  (atom 0)]
       (reify ThreadFactory
         (newThread [_ runnable]
           (let [n (swap! counter inc)
                 t (Thread.
                    (fn []
                      (try
                        (.run ^Runnable runnable)
                        (finally
                          ;; shutdown interrupts an idle thread; a destroy
                          ;; that waits must not see the flag.
                          (Thread/interrupted)
                          (try
                            (run-handler-destroys! (get @handlers workload))
                            (finally
                              (swap! threads disj (Thread/currentThread))))))))]
             (.setName t (str "clj-native-" (name workload) "-" n))
             (.setDaemon t true)
             (swap! threads conj t)
             t))))))

#?(:clj
   (defn- slot-executor
     "The fixed pool of `workload`: (:size registry) threads that run the
      handler inits of `workload` before each task."
     ^ThreadPoolExecutor [registry workload]
     (let [size (int (:size registry))]
       (proxy [ThreadPoolExecutor] [size size 0 TimeUnit/MILLISECONDS
                                    (LinkedBlockingQueue.)
                                    (make-thread-factory registry workload)]
         (beforeExecute [_ _]
           (run-handler-inits! (get @(:handlers registry) workload)))))))

(defn init-workload-pool!
  "Create a workload pool registry.

     :size      JVM: worker count of each slot (availableProcessors).
                CLJS: pool worker count, :auto (default) or an integer.
     :handler-runtime (CLJS) Diagnostic config for pool/init-pool!.

   (CLJS) Read the pool through current-pool."
  [opts]
  #?(:clj
     (let [size (or (:size opts)
                    (.. Runtime getRuntime availableProcessors))]
       {:slots (atom {:mixed   nil
                      :io      nil
                      :compute nil})
        :handlers (atom {:mixed   []
                         :io      []
                         :compute []})
        :threads (atom #{})
        :size size
        :terminated? (atom false)})
     :cljs
     {:pool (atom nil)
      :owned? (atom false)
      :promise (atom nil)
      :stopping (atom nil)
      :handlers (atom [])
      :opts opts
      :terminated? (atom false)}))

(defn- replace-or-append-handler
  "`entries` with `entry` in place of the entry of its lib-key, or added at
   the end."
  [entries entry]
  (let [k (:lib-key entry)]
    (if (some (fn [e] (= (:lib-key e) k)) entries)
      (mapv (fn [e] (if (= (:lib-key e) k) entry e)) entries)
      (conj (vec entries) entry))))

;; The :cljs branch ignores `workload`.
#_{:clj-kondo/ignore [:unused-binding]}
(defn register-handler!
  "Register `spec` for `lib-key` on `registry`. Returns `registry`.

     :args          Init payload for each worker. JVM: passed to :init.
                    CLJS: sent to the worker, so it must be
                    structured-cloneable.
     :init          (JVM) Fn of :args to state, run once on each thread.
     :destroy       (JVM, optional) Fn of state, run at shutdown.
     :module        (CLJS) ES module URL that each worker imports, since a
                    worker cannot receive a closure. Its default, handler or
                    create export takes :args and returns the methods. Its
                    `destroy` export is the teardown. It must not spawn
                    workers, which would sit outside the pool's routing and
                    shutdown.
     :pre-terminate (CLJS) Zero-arg host-side hook that shutdown-pool!
                    runs and awaits.

   `workload` is the JVM slot: :mixed, :io or :compute.

   A second registration of a lib-key replaces its spec and keeps its place
   in the shutdown order.

   CLJS: a spec needs :module or :pre-terminate. A :module spec must
   register before ensure-pool! or adopt-pool!, since a live pool cannot
   import a new module."
  [registry workload lib-key spec]
  #?(:clj
     (do (swap! (:handlers registry) update workload replace-or-append-handler
                (assoc spec :lib-key lib-key))
         registry)
     :cljs
     (let [entry (assoc spec :lib-key lib-key)]
       (when-not (or (:module entry) (:pre-terminate entry))
         (throw (ex-info (str "workload-pool: a CLJS handler spec needs "
                              ":module or :pre-terminate (lib-key "
                              lib-key ")")
                         {:lib-key lib-key})))
       (when (and (:module entry) (some? @(:promise registry)))
         (throw (ex-info (str "workload-pool: the pool already "
                              "exists; a spec with a :module must register "
                              "before ensure-pool!/adopt-pool! (lib-key "
                              lib-key ")")
                         {:lib-key lib-key})))
       (swap! (:handlers registry) replace-or-append-handler entry)
       registry)))

#?(:clj
   (defn current-context
     "Return the state of `lib-key` on this pool thread. Throws the error of
      its init when that failed on this thread. Throws on a non-pool thread,
      or when no handler is registered for `lib-key`."
     [lib-key]
     (let [state (.get thread-handler-state)]
       (when-let [e (get (.get thread-init-errors) lib-key)]
         (throw e))
       (when (empty? state)
         (throw (ex-info
                 (str "clj-native.workload-pool: current-context called "
                      "from a non-pool thread (lib-key " lib-key ")")
                 {:lib-key lib-key})))
       (when-not (contains? state lib-key)
         (throw (ex-info
                 (str "clj-native.workload-pool: no handler registered "
                      "for lib-key " lib-key " on this thread")
                 {:lib-key lib-key
                  :available (keys state)})))
       (get state lib-key))))

#?(:clj
   (defn current-context-or-nil
     "current-context, but nil where it would throw, for a per-call probe
      that cannot afford the ex-info."
     [lib-key]
     (get (.get thread-handler-state) lib-key)))

#?(:cljs
   (defn- ^:async spawn-pool!
     "Fold every spec with a :module into one pool/init-pool! call, and
      record the pool on `registry`. Rejects when no spec has a :module,
      since that pool could run nothing."
     [registry]
     (let [entries @(:handlers registry)
           module-specs (filterv (fn [e] (some? (:module e))) entries)]
       (when (zero? (count module-specs))
         (throw (ex-info (str "workload-pool: no registered handler spec "
                              "carries a :module; register-handler! before "
                              "ensure-pool!")
                         {:registered (mapv :lib-key entries)})))
       (let [handlers (reduce (fn [m e]
                                (assoc m (:lib-key e)
                                       {:module (:module e)
                                        :init   (:args e)}))
                              {}
                              module-specs)
             opts     (:opts registry)
             p        (await (pool/init-pool!
                              {:handlers        handlers
                               :size            (:size opts)
                               :handler-runtime (:handler-runtime opts)}))]
         (reset! (:pool registry) p)
         (reset! (:owned? registry) true)
         p))))

#?(:cljs
   (defn ensure-pool!
     "Return a Promise of the pool, and spawn it on the first call. Every
      caller gets the same Promise. A rejected init and shutdown-pool! clear
      it, so the next call spawns a fresh pool from the registered specs. A
      call during shutdown-pool! waits for the shutdown to end."
     [registry]
     (if-let [stopping @(:stopping registry)]
       (.then stopping (fn [_] (ensure-pool! registry)))
       (or @(:promise registry)
           (let [promise (.catch (spawn-pool! registry)
                                 (fn [err]
                                   (reset! (:promise registry) nil)
                                   (throw err)))]
             (reset! (:terminated? registry) false)
             (reset! (:promise registry) promise)
             promise)))))

#?(:cljs
   (defn adopt-pool!
     "Store pool `p`, created elsewhere, on `registry` with owned? false, and
      make ensure-pool! resolve to it. shutdown-pool! then runs the
      :pre-terminate hooks but leaves `p` up. Throws when a pool is present
      or initializing. Returns `registry`."
     [registry p]
     (when (some? @(:promise registry))
       (throw (ex-info (str "workload-pool: a pool is already present or "
                            "initializing; shutdown-pool! before "
                            "adopt-pool!")
                       {:owned? @(:owned? registry)})))
     (reset! (:pool registry) p)
     (reset! (:owned? registry) false)
     (reset! (:terminated? registry) false)
     (reset! (:promise registry) (js/Promise.resolve p))
     registry))

#?(:cljs
   (defn current-pool
     "Return the live pool of `registry`, or nil when it has none."
     [registry]
     @(:pool registry)))

#?(:clj
   (defn- throw-if-terminated
     [registry workload]
     (when @(:terminated? registry)
       (throw (ex-info "workload-pool: registry already terminated"
                       {:workload workload})))))

#?(:clj
   (defn as-executor-service
     "Return the ExecutorService of the `workload` slot, for a
      core.async.flow :mixed-exec, :io-exec or :compute-exec. The first call
      builds it. Each thread runs the handler inits before its first task.
      Throws after shutdown-pool!."
     [registry workload]
     (throw-if-terminated registry workload)
     (let [slots (:slots registry)]
       (or (get @slots workload)
           ;; Double-checked locking, so two racing callers cannot each build
           ;; an executor and leak one. slots is stable, so a valid monitor.
           #_{:clj-kondo/ignore [:locking-suspicious-lock]}
           (locking slots
             ;; Again under the lock, so close-slots! cannot miss a new slot.
             (throw-if-terminated registry workload)
             (or (get @slots workload)
                 (let [exec (slot-executor registry workload)]
                   (swap! slots assoc workload exec)
                   exec)))))))

#?(:clj
   (defn- close-slots!
     "Mark `registry` terminated and return its executors, or nil when it
      already was. Takes the slots lock, as as-executor-service does."
     [registry]
     (let [slots (:slots registry)]
       #_{:clj-kondo/ignore [:locking-suspicious-lock]}
       (locking slots
         (when-not @(:terminated? registry)
           (reset! (:terminated? registry) true)
           (vec (keep val @slots)))))))

#?(:cljs
   (defn- ^:async run-pre-terminate-hooks!
     "Run and await the :pre-terminate hook of each entry in order. Logs a
      failure and continues, so one library cannot strand the others."
     [entries]
     (doseq [entry entries]
       (let [hook (:pre-terminate entry)]
         (when hook
           (try
             (await (hook))
             (catch :default e
               (js/console.warn "workload-pool: pre-terminate failed for"
                                (str (:lib-key entry)) e))))))))

#?(:cljs
   (defn- ^:async stop-pool!
     "The CLJS shutdown-pool! work. Clears :stopping at the end."
     [registry]
     (let [promise @(:promise registry)
           _       (await (js/Promise.allSettled [promise]))
           p       @(:pool registry)
           owned?  @(:owned? registry)
           entries (vec (reverse @(:handlers registry)))]
       (await (run-pre-terminate-hooks! entries))
       (when (and (some? p) owned?)
         (try
           (await (.terminate p))
           (catch :default e
             ;; The pool dies with its workers anyway. Finish the cleanup.
             (js/console.warn "workload-pool: pool terminate rejected"
                              e))))
       ;; An adopt-pool! during the wait owns a newer pool.
       (when (compare-and-set! (:promise registry) promise nil)
         (reset! (:pool registry) nil)
         (reset! (:owned? registry) false))
       (reset! (:stopping registry) nil)
       registry)))

(defn shutdown-pool!
  "Tear down `registry` and return it (CLJS: a Promise of it). A call after
   the shutdown returns nil.

   JVM: shut down the executors, and join each thread after it runs its
   handler destroys.

   CLJS: wait for a pool that is starting, then run the :pre-terminate hooks
   in reverse registration order, and log a failure. Terminate the pool only
   when the registry owns it. Then remove the pool from the registry. The
   specs stay registered, so a later ensure-pool! spawns a fresh pool. A
   call during the shutdown returns its Promise."
  [registry]
  #?(:clj
     (when-let [execs (close-slots! registry)]
       (let [deadline (+ (System/currentTimeMillis) 60000)
             ms-left  #(max 1 (- deadline (System/currentTimeMillis)))]
         (run! #(.shutdown ^ExecutorService %) execs)
         (doseq [^ExecutorService exec execs]
           (.awaitTermination exec (long (ms-left)) TimeUnit/MILLISECONDS))
         ;; A thread runs its destroys after the executor terminates. Join,
         ;; so the JVM cannot exit while one is still in native teardown.
         (doseq [^Thread t @(:threads registry)]
           (.join t (long (ms-left)))
           (when (.isAlive t)
             (log/warn "workload-pool: shutdown timed out waiting for" (.getName t)))))
       registry)
     :cljs
     (or @(:stopping registry)
         (if @(:terminated? registry)
           (js/Promise.resolve nil)
           (do (reset! (:terminated? registry) true)
               (let [promise (stop-pool! registry)]
                 (reset! (:stopping registry) promise)
                 promise))))))

#?(:cljs
   (defn make-wiring!
     "A holder for the registry of one consumer and the Promise of its
      setup. Make one at load time. The setup builds the registry, so the
      registry cannot guard it, and without this holder two concurrent
      callers each spawn a pool."
     []
     {:registry (atom nil)
      :promise (atom nil)
      :stopping (atom nil)}))

#?(:cljs
   (defn- ^:async run-wiring!
     "The ensure-wired! setup: build the registry, run :register! on it, then
      adopt the :pool of the caller or spawn an owned pool. Resolves to the
      pool."
     [wiring opts]
     (let [reg         (init-workload-pool! (get opts :registry-opts {}))
           register!   (:register! opts)
           caller-pool (:pool opts)]
       (reset! (:registry wiring) reg)
       (when register!
         (await (register! reg)))
       (if (some? caller-pool)
         (do (adopt-pool! reg caller-pool)
             caller-pool)
         (await (ensure-pool! reg))))))

#?(:cljs
   (defn ensure-wired!
     "Return a Promise of the pool of `wiring`, and run its setup on the
      first call. Every caller gets the same Promise, and a later call
      ignores its opts. A rejected setup clears the wiring, so a later call
      retries. Call shutdown-wiring! to start again. A call during
      shutdown-wiring! waits for it, then runs a new setup.

      opts, all optional:
        :registry-opts Map for init-workload-pool!.
        :register!     Fn of the new registry, called before the pool
                       exists so :module specs can register. A returned
                       Promise is awaited, so one-time async setup goes here.
        :pool          A pool of the caller to adopt. Without it, the setup
                       spawns an owned pool."
     [wiring opts]
     (if-let [stopping @(:stopping wiring)]
       (.then stopping (fn [_] (ensure-wired! wiring opts)))
       (or @(:promise wiring)
           (let [promise (.catch (run-wiring! wiring opts)
                                 (fn [err]
                                   (reset! (:registry wiring) nil)
                                   (reset! (:promise wiring) nil)
                                   (throw err)))]
             (reset! (:promise wiring) promise)
             promise)))))

#?(:cljs
   (defn wiring-pool
     "Return the live pool of `wiring`, or nil (never undefined) before
      ensure-wired! resolves and after shutdown-wiring!. A consumer in another
      package must use this and never deref a wiring atom."
     [wiring]
     ;; Explicit nil else: squint compiles a tail `when` to an `if` with no
     ;; else, which returns undefined.
     (if-let [reg @(:registry wiring)]
       (current-pool reg)
       nil)))

#?(:cljs
   (defn live-pool?
     "True when `p` is the pool that `wiring` routes to now. Check the
      recorded pool of a handle here before a GC teardown posts: a new pool
      restarts its worker ids, so a stale teardown frees a live handle. The
      check uses identity, so the teardowns of an adopted pool resume once it
      is adopted again; one between the shutdown and the re-adoption leaks."
     [wiring p]
     (and (some? p) (identical? p (wiring-pool wiring)))))

#?(:cljs
   (defn- ^:async stop-wiring!
     "The shutdown-wiring! work. Clears :stopping at the end."
     [wiring]
     (await (js/Promise.allSettled [@(:promise wiring)]))
     (let [reg @(:registry wiring)]
       (when (some? reg)
         (await (shutdown-pool! reg))
         (reset! (:registry wiring) nil)
         (reset! (:promise wiring) nil))
       (reset! (:stopping wiring) nil)
       reg)))

#?(:cljs
   (defn shutdown-wiring!
     "Wait for a setup that is running, run shutdown-pool! on the registry
      of `wiring`, and clear the wiring so a later ensure-wired! starts
      over. Resolves to that registry, or to nil when the wiring held none,
      so a consumer can clean up the never-wired case itself. A call during
      the shutdown returns its Promise."
     [wiring]
     (or @(:stopping wiring)
         (let [promise (stop-wiring! wiring)]
           (reset! (:stopping wiring) promise)
           promise))))
