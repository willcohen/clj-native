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
      runs the handler inits of its slot once and keeps the state in a
      thread-local
      that current-context reads.

      CLJS: one worker-router joint pool for all libraries, so a
      cross-library pipeline stays on one worker. The workload argument is
      ignored, because JS work holds a thread only while it computes.

      A CLJS consumer that builds its registry during init uses a wiring
      (make-wiring!, ensure-wired!), because the registry latch cannot guard
      the pass that creates it."
     (:require [clojure.tools.logging :as log])
     (:import [java.util.concurrent
               ExecutorService
               Executors
               ThreadFactory
               TimeUnit]))
   :cljs
   (ns net.willcohen.native.workload-pool
     "Worker pools: one worker-router joint pool shared by every library
      that calls register-handler!."
     (:require ["./pool.mjs" :as pool])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (def ^:private ^ThreadLocal thread-handler-state
     ;; {lib-key -> handler-state}. Empty on a non-pool thread, which is how
     ;; current-context detects one.
     (proxy [ThreadLocal] []
       (initialValue [] {}))))

#?(:clj
   (defn- ensure-thread-handler-state-bound!
     "Run each handler init on this thread once for each lib-key. Returns
      the state map."
     [handlers]
     (let [current (.get thread-handler-state)
           updated (reduce
                    (fn [acc {:keys [lib-key init args]}]
                      (if (contains? acc lib-key)
                        acc
                        (let [state (if init (init args) nil)]
                          (assoc acc lib-key state))))
                    current
                    handlers)]
       (when-not (identical? current updated)
         (.set thread-handler-state updated))
       updated)))

#?(:clj
   (defn- destroy-thread-handler-state!
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
;; A CAS on a weak-keyed sentinel for each pointer stops the double free.

#?(:clj
   (def ^:private ^java.util.Map released-sentinels
     (java.util.Collections/synchronizedMap (java.util.WeakHashMap.))))

#?(:clj
   (defn- get-or-make-sentinel!
     ^java.util.concurrent.atomic.AtomicBoolean [pointer]
     (locking released-sentinels
       (or (.get released-sentinels pointer)
           (let [s (java.util.concurrent.atomic.AtomicBoolean. false)]
             (.put released-sentinels pointer s)
             s)))))

#?(:clj
   (defn release-once!
     "Run zero-arg `release-fn` at most once for each `pointer`, across
      threads, while the first `pointer` object passed stays reachable (the
      sentinels have weak keys). Pass a dt-ffi Pointer or a pointer record,
      not a long: a long boxes anew on each call, so after a GC release-fn
      can run again. Returns true when it ran here, else nil.

      With a non-nil `lock`, release-fn also runs under that lock. Pass one
      lock for each library with process-global state, where the destroys of
      two different pointers can race into a SIGSEGV."
     ([pointer release-fn]
      (release-once! pointer nil release-fn))
     ([pointer lock release-fn]
      (when pointer
        (let [sentinel (get-or-make-sentinel! pointer)]
          (when (.compareAndSet sentinel false true)
            (if lock
              (locking lock (release-fn))
              (release-fn))
            true))))))

#?(:clj
   (defn- make-thread-factory
     "A ThreadFactory of daemon threads named clj-native-<slot>-<n>. Each
      thread runs the handler inits for `workload` before its first job. It
      reads `handlers-atom` at thread start, so a handler that registers
      after as-executor-service still runs on later threads."
     [slot-name handlers-atom workload]
     (let [counter (atom 0)]
       (reify ThreadFactory
         (newThread [_ runnable]
           (let [n (swap! counter inc)
                 t (Thread.
                    (fn []
                      (let [handlers (get @handlers-atom workload)]
                        (try
                          (ensure-thread-handler-state-bound! handlers)
                          (.run ^Runnable runnable)
                          (finally
                            (destroy-thread-handler-state! handlers))))))]
             (.setName t (str "clj-native-" slot-name "-" n))
             (.setDaemon t true)
             t))))))

(defn init-workload-pool!
  "Create a workload pool registry.

     :size      JVM: default worker count for each slot (availableProcessors).
                CLJS: pool worker count, :auto (default) or an integer.
     :sizes     (JVM) Worker count for each slot, such as {:io 32}. A
                missing slot uses :size.
     :handler-runtime (CLJS) Diagnostic config for pool/init-pool!.

   The returned map is API, and the workload-pool suites pin it.
   JVM: :slots and :handlers atoms keyed :mixed, :io and :compute; :size;
   :sizes; :runtime :jvm; a :terminated? atom.
   CLJS: atoms :pool, :owned?, :latch (the init promise), :generation,
   :handlers (a vector in registration order) and :terminated?; :opts;
   :runtime :cljs. A consumer in another package reads the pool through
   current-pool, never (:pool registry)."
  [opts]
  #?(:clj
     (let [size (or (:size opts)
                    (.. Runtime getRuntime availableProcessors))
           per-slot (:sizes opts)
           sizes (into {} (for [slot [:mixed :io :compute]]
                            [slot (or (get per-slot slot) size)]))]
       {:slots (atom {:mixed   nil
                      :io      nil
                      :compute nil})
        :handlers (atom {:mixed   []
                         :io      []
                         :compute []})
        :size size
        :sizes sizes
        :runtime :jvm
        :terminated? (atom false)})
     :cljs
     {:pool (atom nil)
      :owned? (atom false)
      :latch (atom nil)
      :generation (atom 0)
      :handlers (atom [])
      :opts opts
      :runtime :cljs
      :terminated? (atom false)}))

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

   CLJS: a spec needs :module or :pre-terminate. A :module spec must
   register before ensure-pool! or adopt-pool!, since a live pool cannot
   import a new module. A second registration of a lib-key replaces its spec
   and keeps its place in the shutdown order."
  [registry workload lib-key spec]
  #?(:clj
     (let [entry (assoc spec :lib-key lib-key)]
       (swap! (:handlers registry) update workload (fnil conj []) entry)
       registry)
     :cljs
     (let [entry (assoc spec :lib-key lib-key)]
       (when-not (or (:module entry) (:pre-terminate entry))
         (throw (ex-info (str "workload-pool: a CLJS handler spec needs "
                              ":module or :pre-terminate (lib-key "
                              lib-key ")")
                         {:lib-key lib-key})))
       (when (and (:module entry) (some? @(:latch registry)))
         (throw (ex-info (str "workload-pool: the joint pool already "
                              "exists; a spec with a :module must register "
                              "before ensure-pool!/adopt-pool! (lib-key "
                              lib-key ")")
                         {:lib-key lib-key})))
       (swap! (:handlers registry)
              (fn [entries]
                (if (some (fn [e] (= (:lib-key e) lib-key)) entries)
                  (mapv (fn [e] (if (= (:lib-key e) lib-key) entry e))
                        entries)
                  (conj entries entry))))
       registry)))

(defn current-context
  "Return the state for `lib-key` on this pool thread. Throws on a non-pool
   thread, or when no handler is registered for `lib-key`.

   CLJS always throws: worker state lives inside the worker, and the handler
   module reaches it there."
  [lib-key]
  #?(:clj
     (let [state (.get thread-handler-state)]
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
       (get state lib-key))
     :cljs
     (throw (ex-info
             (str "clj-native.workload-pool: current-context is not "
                  "callable from the main thread on CLJS (lib-key "
                  lib-key "). Per-worker state lives inside Web Workers "
                  "and is unreachable synchronously; worker-side code "
                  "reaches its own state through the worker-router "
                  "handler module.")
             {:lib-key lib-key}))))

#?(:clj
   (defn current-context-or-nil
     "current-context, but nil where it would throw, for a per-call probe
      that cannot afford the ex-info."
     [lib-key]
     (let [state (.get thread-handler-state)]
       (when state
         (get state lib-key)))))

#?(:cljs
   (defn- ^:async spawn-joint-pool!
     "Fold every spec with a :module into one pool/init-pool! call, and
      record the pool on `registry`. Rejects when no spec has a :module,
      since that pool could run nothing."
     [registry]
     (let [entries @(:handlers registry)
           modular (filterv (fn [e] (some? (:module e))) entries)]
       (when (zero? (count modular))
         (throw (ex-info (str "workload-pool: no registered handler spec "
                              "carries a :module; register-handler! before "
                              "ensure-pool!")
                         {:registered (mapv (fn [e] (:lib-key e)) entries)})))
       (let [handlers (reduce (fn [m e]
                                (assoc m (:lib-key e)
                                       {:module (:module e)
                                        :init   (:args e)}))
                              {}
                              modular)
             opts     (:opts registry)
             result   (await (pool/init-pool!
                              {:handlers        handlers
                               :size            (:size opts)
                               :handler-runtime (:handler-runtime opts)}))
             p        (.-pool result)]
         (reset! (:pool registry) p)
         (reset! (:owned? registry) (.-owned result))
         (reset! (:terminated? registry) false)
         p))))

#?(:cljs
   (defn ensure-pool!
     "Return a Promise of the joint pool, and spawn it on the first call.
      Every caller gets the same latched promise. A rejected init and
      shutdown-pool! clear the latch, so the next call spawns a fresh pool
      from the registered specs."
     [registry]
     (or @(:latch registry)
         ;; spawn-joint-pool! runs synchronously to its first await, so no
         ;; other caller can read the latch as nil before this reset!.
         (let [promise (.catch (spawn-joint-pool! registry)
                               (fn [err]
                                 (reset! (:latch registry) nil)
                                 (throw err)))]
           (reset! (:latch registry) promise)
           promise))))

#?(:cljs
   (defn adopt-pool!
     "Store pool `p`, created elsewhere, on `registry` with owned? false, and
      make ensure-pool! resolve to it. shutdown-pool! then runs the
      :pre-terminate hooks but leaves `p` up. Throws when a pool is present
      or initializing. Returns `registry`."
     [registry p]
     (when (some? @(:latch registry))
       (throw (ex-info (str "workload-pool: a pool is already present or "
                            "initializing; shutdown-pool! before "
                            "adopt-pool!")
                       {:owned? @(:owned? registry)})))
     (reset! (:pool registry) p)
     (reset! (:owned? registry) false)
     (reset! (:terminated? registry) false)
     (reset! (:latch registry) (js/Promise.resolve p))
     registry))

#?(:cljs
   (defn current-pool
     "Return the live pool of `registry`, or nil when it has none."
     [registry]
     @(:pool registry)))

#?(:clj
   (defn as-executor-service
     "Return the ExecutorService of the `workload` slot, for a
      core.async.flow :mixed-exec, :io-exec or :compute-exec. The first call
      builds it. Each thread runs the handler inits before its first task.
      Throws after shutdown-pool!."
     [registry workload]
     (when @(:terminated? registry)
       (throw (ex-info "workload-pool: registry already terminated"
                       {:workload workload})))
     (let [slots (:slots registry)]
       (or (get @slots workload)
           ;; Double-checked locking, so two racing callers cannot each build
           ;; an executor and leak one. slots is stable, so a valid monitor.
           #_{:clj-kondo/ignore [:locking-suspicious-lock]}
           (locking slots
             (or (get @slots workload)
                 (let [size (or (get (:sizes registry) workload) (:size registry))
                       factory (make-thread-factory (name workload)
                                                    (:handlers registry)
                                                    workload)
                       exec (Executors/newFixedThreadPool size factory)]
                   (swap! slots assoc workload exec)
                   exec)))))))

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

(defn ^:async shutdown-pool!
  "Tear down `registry` and return it (CLJS: a Promise of it). A second call
   returns nil.

   JVM: run every handler destroy on each thread, then shut down and join
   the executors.

   CLJS: run the :pre-terminate hooks in reverse registration order, and log
   a failure. Terminate the pool only when the registry owns it. Clear :pool
   and :latch, and increment :generation. The specs stay registered, so a
   later ensure-pool! spawns a fresh pool."
  [registry]
  (when-not @(:terminated? registry)
    (reset! (:terminated? registry) true)
    #?(:clj
       (let [slots @(:slots registry)
             handlers @(:handlers registry)]
         (doseq [[workload exec] slots]
           (when exec
             ;; One destroy task for each thread of this slot, so every thread
             ;; runs its destroy before it stops.
             (let [size (or (get (:sizes registry) workload) (:size registry))]
               (dotimes [_ size]
                 (.submit ^ExecutorService exec
                          ^Runnable
                          (fn []
                            (destroy-thread-handler-state!
                             (get handlers workload)))))
               (.shutdown ^ExecutorService exec)
               ;; Join, so the JVM cannot exit while a worker is still in
               ;; native teardown.
               (when-not (.awaitTermination ^ExecutorService exec
                                            60 TimeUnit/SECONDS)
                 (log/warn "workload-pool: shutdown timed out waiting for"
                           workload "destroys to drain")))))
         registry)
       :cljs
       (let [p       @(:pool registry)
             owned?  @(:owned? registry)
             entries (vec (reverse @(:handlers registry)))]
         (await (run-pre-terminate-hooks! entries))
         (when (and (some? p) owned?)
           (try
             (await (pool/terminate-pool! p))
             (catch :default e
               ;; The pool dies with its workers anyway. Finish the cleanup.
               (js/console.warn "workload-pool: pool terminate rejected"
                                e))))
         (reset! (:pool registry) nil)
         (reset! (:owned? registry) false)
         (reset! (:latch registry) nil)
         (swap! (:generation registry) inc)
         registry))))

#?(:cljs
   (defn make-wiring!
     "A holder for the registry of one consumer and the Promise of its
      setup. Make one at load time. The setup builds the registry, so the
      registry cannot guard it, and without this holder two concurrent
      callers each spawn a pool."
     []
     {:registry (atom nil)
      :latch (atom nil)}))

#?(:cljs
   (defn- ^:async run-wiring!
     "The ensure-wired! setup: build the registry, run :register! on it, then
      adopt the :pool of the caller or spawn an owned pool. Resolves to the
      pool."
     [wiring opts]
     (let [reg         (init-workload-pool! (or (:registry-opts opts) {}))
           register!   (:register! opts)
           caller-pool (:pool opts)]
       ;; Before the first await, so wiring-pool and shutdown-wiring! see a
       ;; running pass.
       (reset! (:registry wiring) reg)
       (when register!
         (await (register! reg)))
       (if (some? caller-pool)
         (do (adopt-pool! reg caller-pool)
             caller-pool)
         (await (ensure-pool! reg))))))

#?(:cljs
   (defn ensure-wired!
     "Return a Promise of the joint pool of `wiring`, and run the wiring pass
      on the first call. Every caller gets the same latched promise, and a
      later call ignores its opts. A rejected pass clears the wiring, so a
      later call retries. Call shutdown-wiring! to start again.

      opts, all optional:
        :registry-opts Map for init-workload-pool!.
        :register!     Fn of the new registry, called before the pool
                       exists so :module specs can register. A returned
                       Promise is awaited, so one-time async setup goes here.
        :pool          A pool of the caller to adopt. Without it, the setup
                       spawns an owned pool."
     [wiring opts]
     (or @(:latch wiring)
         ;; run-wiring! runs synchronously to its first await, so no other
         ;; caller can read the latch as nil before this reset!.
         (let [promise (.catch (run-wiring! wiring opts)
                               (fn [err]
                                 (reset! (:registry wiring) nil)
                                 (reset! (:latch wiring) nil)
                                 (throw err)))]
           (reset! (:latch wiring) promise)
           promise))))

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
     "True when `p` is the pool that `wiring` routes to now.

      A consumer that tracks native handles records the pool of each handle
      and checks it here before it posts a teardown. A GC callback can fire
      after shutdown-wiring! and ensure-wired! replace the pool. The new pool
      restarts its worker ids, so a stale teardown would free a live handle.
      Drop it: its memory died with the old workers.

      The check uses pool identity, not :generation, so the teardowns of an
      adopted pool run again once the consumer adopts it again. A teardown
      between shutdown-wiring! and that re-adoption is dropped, and its
      memory leaks."
     [wiring p]
     (and (some? p) (identical? p (wiring-pool wiring)))))

#?(:cljs
   (defn ^:async shutdown-wiring!
     "Run shutdown-pool! on the registry of `wiring`, and clear the wiring so
      a later ensure-wired! starts over. Resolves to that registry, or to nil
      when the wiring held none, so a consumer can clean up the never-wired
      case itself."
     [wiring]
     (if-let [reg @(:registry wiring)]
       (do (await (shutdown-pool! reg))
           (reset! (:registry wiring) nil)
           (reset! (:latch wiring) nil)
           reg)
       nil)))
