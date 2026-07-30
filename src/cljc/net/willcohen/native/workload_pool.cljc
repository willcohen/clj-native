;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#?(:clj
   (ns net.willcohen.native.workload-pool
     "Workload-class executor pool registry (JVM), and joint worker-pool
      registry (CLJS). There is one cross-platform surface. Libraries
      register handler specs for each worker with register-handler!. The
      platform then decides what it materializes.

      JVM. Three slots (:mixed, :io, :compute) mirror the core.async.flow
      :mixed-exec, :io-exec and :compute-exec. Each slot is an
      ExecutorService. Its ThreadFactory runs handler init one time on each
      fresh thread. The ThreadFactory also puts the state of each library
      on a thread-local, and current-context reads that thread-local.

      CLJS. There is ONE worker-router joint pool, and every registered
      library shares it. Thus a cross-library pipeline stays on one worker,
      and payloads never cross worker boundaries.

      CLJS accepts the workload argument and ignores it. The trichotomy here
      is :mixed, :io and :compute. It is NOT the three dispatch backends of
      net.willcohen.native.dispatch, which are a separate three. This
      trichotomy classifies how work occupies a thread. The only occupancy
      in JS is synchronous compute, because an await on I/O holds no thread,
      on a worker thread as well.

      ensure-pool! folds every registered spec into one pool/init-pool!
      call. It is async and latched. adopt-pool! stores a pool that
      something else created, and sets owned? to false. shutdown-pool! runs
      the :pre-terminate hook of each handler, in reverse registration
      order. It terminates the pool only when the registry owns it, and
      then it increments :generation.

      CLJS wirings. The wiring pass creates a registry, thus the latch of
      that registry cannot make the pass run one time only. A wiring
      (make-wiring!) holds the registry and the memo for the whole pass.
      ensure-wired! latches registry creation, handler registration and the
      spawn-or-adopt as ONE unit. Consumers reach the pool through
      wiring-pool. They guard a captured pool ref with live-pool?."
     (:require [clojure.tools.logging :as log])
     (:import [java.util.concurrent
               ExecutorService
               Executors
               ThreadFactory
               TimeUnit]))
   :cljs
   (ns net.willcohen.native.workload-pool
     "Workload-class executor pool registry. Refer to the JVM namespace
      docstring."
     (:require ["./pool.mjs" :as pool])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (def ^:private ^ThreadLocal thread-handler-state
     ;; {lib-key -> handler-state} for the current worker thread. The first
     ;; task on a fresh thread populates it. It stays empty on the caller
     ;; thread, which is not a pool worker. current-context uses that fact to
     ;; detect the caller thread.
     (proxy [ThreadLocal] []
       (initialValue [] {}))))

#?(:clj
   (defn- ensure-thread-handler-state-bound!
     "Run the init of each registered handler on this thread one time, keyed
      by lib-key. This function is idempotent. Returns the updated state map."
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
     "Run each registered destroy on the stored state of this thread. The
      order is the reverse of registration, thus a later library unwinds
      before an earlier one. Clears the ThreadLocal."
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

;; Release-once gate. The explicit release (handler.destroy) and the GC
;; dispose-fn call the same native destructor. Without coordination, the second
;; call double-frees. release-once! gates with a CAS on an AtomicBoolean
;; sentinel for each pointer, with weak keys. This mirrors the
;; fired?/compare-and-set! pattern in the CLJS pool.

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
     "Run `release-fn`, a zero-argument fn, one time at most for each
      `pointer` identity, across concurrent callers. Returns true if and only
      if the release fired here. `pointer` tags a native handle. It is a
      dt-ffi Pointer, or the raw address as a long.

      The 3-argument arity also serializes release-fn under `lock`. The CAS
      for each pointer stops a double-free of ONE pointer. But some libraries
      hold process-global state, and two DIFFERENT pointers can race into a
      SIGSEGV there. Pass one lock for each such library, so its destroys run
      one at a time. A nil lock means no synchronization."
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
     "Build a ThreadFactory. Its threads are daemon threads, thus they do not
      block JVM exit. Each thread has the name clj-native-<slot>-<n>. Each
      thread runs the init of every registered handler one time, on its first
      job.

      `workload` keys into `handlers-atom`, which is the :handlers of the
      registry. The thread body reads that atom again on entry. Thus a handler
      that registers after as-executor-service still fires its init on a
      thread that does not exist yet."
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
  "Create a workload pool registry. Returns a handle for register-handler!.
   On the JVM the handle also serves as-executor-service. On CLJS it serves
   ensure-pool!, adopt-pool!, current-pool and shutdown-pool!.

   A CLJS consumer in an OTHER package reads the live pool through
   current-pool. Such a consumer must never deref (:pool registry). Refer to
   the current-pool docstring.

     :size      JVM: the default worker count for each slot. The default is
                availableProcessors.
                CLJS: the worker count for the joint pool, :auto or an
                integer. An absent value means :auto.
     :sizes     (JVM, optional) The worker count for each slot, for example
                {:io 32 :mixed 8}. An absent slot uses :size. Thus you can
                size one slot apart, such as an :io slot larger than
                :compute, with no change to the default.
     :handler-runtime (CLJS, optional) The diagnostic config. ensure-pool!
                forwards it to pool/init-pool! when it spawns the pool.

   The shape of the returned registry map is API, and the workload-pool
   suites pin it on purpose.

   JVM: a :slots atom keyed :mixed, :io and :compute, a :handlers atom of one
   vector for each slot, :size, :sizes, :runtime :jvm, and a :terminated?
   atom.

   CLJS (one joint pool): a :pool atom, an :owned? atom, a :latch atom with
   the in-flight or resolved init promise, a :generation atom, a :handlers
   atom with a flat vector in registration order, :opts unchanged, :runtime
   :cljs, and a :terminated? atom. shutdown-pool! increments :generation, so
   a consumer can detect a stale pool ref.

   A rename or a restructure of these keys is a breaking change. Change the
   pins with it, on purpose."
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

;; The :cljs branch never reads `workload`. Refer to the docstring.
#_{:clj-kondo/ignore [:unused-binding]}
(defn register-handler!
  "Register a handler for each worker on `registry`, under `lib-key`. There
   is one spec map, and each key has one meaning on the two platforms:

     :args          The init payload for each worker. On the JVM,
                    register-handler! passes it to :init. On CLJS it
                    forwards the payload to worker-router as the handler
                    init payload, which is a structured-cloneable JS
                    object.
     :init          (JVM) A fn from :args to state. It runs one time on
                    each worker thread. A CLJS worker cannot receive a
                    closure, because structured clone sends data and never
                    code. Thus the worker-side init lives in :module.
     :destroy       (JVM, optional) A fn from state to nil. It runs at
                    shutdown. The CLJS teardown is the module-level
                    `destroy` sibling that worker-bootstrap finds in
                    :module.
     :module        (CLJS) The ES module URL that each worker imports. Its
                    default, handler or create export receives :args, and
                    returns the exposed methods. A handler module must not
                    spawn its own workers. The worker count of the joint
                    pool gives the parallelism. A sub-worker would sit
                    outside the routing, the affinity and the shutdown walk
                    of the pool.
     :pre-terminate (CLJS) A zero-argument host-side hook. shutdown-pool!
                    runs it before the pool terminates, in reverse
                    registration order. shutdown-pool! awaits a returned
                    Promise.

   `workload` names the JVM executor slot (:mixed, :io or :compute). CLJS
   accepts it and ignores it, because every handler folds into the one joint
   pool. Refer to the namespace docstring.

   CLJS rules. A spec must carry :module or :pre-terminate. A spec with a
   :module must register before ensure-pool! or adopt-pool!, because a live
   pool cannot import a new module. A spec with :pre-terminate only can
   register at any time. A second registration of a lib-key replaces its spec
   in place, and the spec keeps its position in the shutdown walk."
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
  "Return the state for `lib-key` on the current thread (JVM) or the current
   worker (CLJS).

   JVM. Reads the ThreadLocal map of the ExecutorService. Throws when the
   caller thread is not a pool worker. Also throws when no handler is
   registered.

   CLJS. Always throws, by design. The state of each worker lives inside a
   Web Worker, and the main thread cannot reach it synchronously. Worker-side
   code reaches its own state through the worker-router handler module."
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
     "current-context, but nil when the caller thread is not a pool worker
      or no handler is registered for `lib-key`. A per-call scope probe
      (does this thread own pooled state?) cannot afford the ex-info that
      current-context throws off-pool. JVM only."
     [lib-key]
     (let [state (.get thread-handler-state)]
       (when state
         (get state lib-key)))))

#?(:cljs
   (defn- ^:async spawn-joint-pool!
     "Fold every registered handler spec with a :module into ONE
      pool/init-pool! call. Then record the pool on the registry. The :args
      of the spec becomes the init payload for that handler in the
      substrate.

      Rejects when no registered spec carries a :module. A pool with no
      handler module can run nothing. A silent empty pool is the defect
      class that this registry exists to delete."
     [registry]
     (let [entries @(:handlers registry)
           modular (filterv (fn [e] (some? (:module e))) entries)]
       (when (zero? (count modular))
         (throw (ex-info (str "workload-pool: no registered handler spec "
                              "carries a :module; register-handler! before "
                              "ensure-pool!")
                         {:registered (mapv (fn [e] (:lib-key e)) entries)})))
       ;; Use reduce, and not assoc into a literal. squint inlines assoc on a
       ;; map that it infers as a literal, and it drops the brackets of the
       ;; computed key. The reduce accumulator has no inferred type, thus it
       ;; stays a runtime assoc.
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
     "Return the Promise of the one joint pool. The first call spawns that
      pool.

      This function is async and latched. The first caller stores the init
      promise on :latch. Every other caller, concurrent or later, gets that
      same promise, which resolves to the pool ref. A rejected init clears
      the latch, so a later call can try again.

      After shutdown-pool! the latch is empty again. Thus the next call
      spawns a fresh pool, with a new :generation, from the handler specs
      that stay registered."
     [registry]
     (or @(:latch registry)
         ;; spawn-joint-pool! is async. Thus its body runs to the first await
         ;; before this tick yields, and that includes the no-module throw.
         ;; The reset! below therefore publishes the latch before another
         ;; caller can see it as nil.
         (let [promise (.catch (spawn-joint-pool! registry)
                               (fn [err]
                                 (reset! (:latch registry) nil)
                                 (throw err)))]
           (reset! (:latch registry) promise)
           promise))))

#?(:cljs
   (defn adopt-pool!
     "Store a worker-router pool that something else created on `registry`,
      with owned? false. shutdown-pool! then runs the :pre-terminate hooks,
      but it leaves the pool up, because the creator owns the lifecycle.

      The latch resolves to the adopted pool. Thus ensure-pool! callers
      compose unchanged. Throws when a pool is present already, or while a
      pool initializes. Call shutdown-pool! first in that case. Returns the
      registry."
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
     "Return the live joint pool of the registry. Returns nil before
      ensure-pool! or adopt-pool!, and after shutdown-pool!.

      A cross-package consumer must read the pool through this fn. Such a
      consumer must never deref (:pool registry) itself. The squint-cljs
      instance of THIS package creates the registry atoms. squint protocols
      are symbols for each instance. Thus a consumer with its own
      squint-cljs copy cannot deref a foreign Atom, and it gets an IDeref
      protocol miss at run time.

      The same constraint covers every atom in the registry map."
     [registry]
     @(:pool registry)))

#?(:clj
   (defn as-executor-service
     "Build an ExecutorService for the `workload` slot in `registry`. It
      serves as a :mixed-exec, an :io-exec or a :compute-exec for
      core.async.flow. It is idempotent for each workload, until shutdown.
      Its ThreadFactory runs every registered handler at thread creation.
      Thus the state of each worker is warm before the first task."
     [registry workload]
     (when @(:terminated? registry)
       (throw (ex-info "workload-pool: registry already terminated"
                       {:workload workload})))
     (let [slots (:slots registry)]
       (or (get @slots workload)
           ;; Double-checked locking on the slots atom. Without it, two
           ;; callers can each see no slot, and each build an
           ;; ExecutorService. The pool of the loser then leaks, because
           ;; nothing shuts it down. slots is the stable atom of this
           ;; registry instance, and thus a valid monitor.
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
     "Run the :pre-terminate hook of each entry in sequence, and await each
      one. A hook failure goes to the log. It does not stop the rest of the
      walk, or the terminate after the walk. One library that does not come
      down must not strand the others."
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
  "Tear down `registry`. After the return, the registry rejects work. On
   CLJS the return is a Promise resolve. A second call does nothing and
   gives nil.

   JVM. Run every registered destroy on every active slot. Then close the
   ExecutorServices.

   CLJS. Run the :pre-terminate hook of each handler, in reverse
   registration order. shutdown-pool! awaits each hook, logs a failure, and
   does not rethrow it. Then terminate the pool ONLY when this registry owns
   it. An adopted pool stays up, because its creator owns the lifecycle.
   Then clear :pool and :latch, and increment :generation.

   The handler specs stay registered. Thus a later ensure-pool! spawns a
   fresh generation. Resolves to the registry."
  [registry]
  (when-not @(:terminated? registry)
    (reset! (:terminated? registry) true)
    #?(:clj
       (let [slots @(:slots registry)
             handlers @(:handlers registry)]
         (doseq [[workload exec] slots]
           (when exec
             ;; Queue one destroy task for each pool thread, so each thread
             ;; fires its destroy before it stops. shutdown then drains the
             ;; queue and stops the threads. Use the size of each slot, so a
             ;; slot with a size apart from :compute drains its own count.
             (let [size (or (get (:sizes registry) workload) (:size registry))]
               (dotimes [_ size]
                 (.submit ^ExecutorService exec
                          ^Runnable
                          (fn []
                            (destroy-thread-handler-state!
                             (get handlers workload)))))
               (.shutdown ^ExecutorService exec)
               ;; Join. Block until every thread runs its destroy and stops.
               ;; Thus the main thread cannot reach JVM exit while a worker
               ;; is still inside native teardown. The release-once! lock
               ;; serializes the destroys of each library. This join closes
               ;; the window between destroy and process exit.
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
               ;; A pool that does not come down dies with its workers in any
               ;; case. The registry must still complete its cleanup.
               (js/console.warn "workload-pool: pool terminate rejected"
                                e))))
         (reset! (:pool registry) nil)
         (reset! (:owned? registry) false)
         (reset! (:latch registry) nil)
         (swap! (:generation registry) inc)
         registry))))

#?(:cljs
   (defn make-wiring!
     "Create a wiring. A wiring is the holder of one consumer for a
      joint-pool registry, with the memo that makes the wiring pass run one
      time. Make one wiring for each consumer namespace, at load time.

      The pass builds the registry INSIDE itself, because the :opts of the
      registry are the init options of the caller. Thus the latch of the
      registry cannot guard the pass that creates it. Two concurrent callers
      would each build a registry, and each spawn a pool. The orphaned pool
      would then keep its workers and their native heaps alive. The wiring
      outlives the registry, and it carries the memo instead.

      The shape of the returned map is API, and the workload-pool suites pin
      it. It has a :registry atom with the live registry, which is nil
      outside a wiring pass. It also has a :latch atom with the in-flight or
      resolved pass promise."
     []
     {:registry (atom nil)
      :latch (atom nil)}))

#?(:cljs
   (defn- ^:async run-wiring!
     "One wiring pass. Build the registry. Then let the consumer register its
      specs on that registry. Then adopt the pool of the caller, or spawn an
      owned pool. Resolves to the pool ref."
     [wiring opts]
     (let [reg         (init-workload-pool! (or (:registry-opts opts) {}))
           register!   (:register! opts)
           caller-pool (:pool opts)]
       ;; Publish the registry before the first await, so wiring-pool and
       ;; shutdown-wiring! can see a pass that is already running.
       (reset! (:registry wiring) reg)
       (when register!
         (await (register! reg)))
       (if (some? caller-pool)
         (do (adopt-pool! reg caller-pool)
             caller-pool)
         (await (ensure-pool! reg))))))

#?(:cljs
   (defn ensure-wired!
     "Return the Promise of the joint pool of `wiring`. The first call runs
      the wiring pass.

      This function is latched. The first caller stores the pass promise on
      the wiring. Every other caller, concurrent or later, gets that same
      promise. A rejected pass clears the wiring, so a later call tries
      again.

      opts keys, all optional:

        :registry-opts A map for init-workload-pool!, unchanged (:size,
                       :handler-runtime).
        :register!     A fn of the new registry. ensure-wired! calls it
                       before the pool exists, so a spec with a :module can
                       still register. ensure-wired! awaits a returned
                       Promise. Thus the async setup of a consumer belongs
                       here, and it also runs one time for each wiring. One
                       example is a read of an init payload from disk.
        :pool          A pool from the caller, for adoption. Its lifecycle
                       stays with the caller. Without this key,
                       ensure-wired! spawns an owned pool through
                       ensure-pool!.

      A later call ignores its opts and gives the pool of the first call,
      which is what a memo means. Call shutdown-wiring! to start again."
     [wiring opts]
     (or @(:latch wiring)
         ;; run-wiring! is async. Thus its body runs to the first await
         ;; before this tick yields. The reset! below therefore publishes the
         ;; latch before another caller can see it as nil.
         (let [promise (.catch (run-wiring! wiring opts)
                               (fn [err]
                                 (reset! (:registry wiring) nil)
                                 (reset! (:latch wiring) nil)
                                 (throw err)))]
           (reset! (:latch wiring) promise)
           promise))))

#?(:cljs
   (defn wiring-pool
     "Return the live joint pool of `wiring`. Returns nil before
      ensure-wired! resolves, and after shutdown-wiring!.

      A consumer in an OTHER package must read the pool through this fn. The
      squint-cljs instance of THIS package creates the wiring atoms. squint
      protocols are symbols for each instance. Thus a consumer with its own
      squint-cljs copy cannot deref a foreign Atom.

      The else branch is written out for one reason. squint compiles a
      `when` in tail position to an `if` with no else. The fn then reaches
      its end with no return value, and it gives undefined and not null. A
      caller with a strict comparison against null sees that difference."
     [wiring]
     (if-let [reg @(:registry wiring)]
       (current-pool reg)
       nil)))

#?(:cljs
   (defn live-pool?
     "Returns true if and only if `p` is the pool that `wiring` routes to at
      this time.

      A consumer that tracks native handles must capture the pool of each
      handle. It must then check that pool here, before it posts the
      teardown of the handle. The host delivers a garbage-collector callback
      at a time of its own choice. That time can come after a
      shutdown-wiring! and ensure-wired! cycle replaces the pool. Each fresh
      pool restarts its worker id sequences. Thus a teardown with a route to
      the new pool frees a LIVE handle.

      A stale teardown is safe to drop. The terminated pool owned the native
      memory, thus that memory went away with its workers.

      The guard keys on pool IDENTITY, and never on the :generation counter
      of the registry. An ADOPTED pool survives shutdown-wiring! with its
      workers and their id sequences intact. Thus the teardowns of its
      handles must still fire after the consumer adopts it again. A counter
      that increases on each shutdown drops those teardowns, and that leaks
      native memory."
     [wiring p]
     (and (some? p) (identical? p (wiring-pool wiring)))))

#?(:cljs
   (defn ^:async shutdown-wiring!
     "Tear down `wiring`. Run shutdown-pool! on its registry. Then clear the
      registry and the memo, so a later ensure-wired! starts a fresh pass.

      Resolves to the registry that came down. Resolves to nil when the
      wiring held no registry. Thus a consumer can run its own fallback
      cleanup for the never-wired case."
     [wiring]
     (if-let [reg @(:registry wiring)]
       (do (await (shutdown-pool! reg))
           (reset! (:registry wiring) nil)
           (reset! (:latch wiring) nil)
           reg)
       nil)))
