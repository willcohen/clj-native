;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.graal-wasm
  "JVM-side WASM-on-GraalVM primitives. This namespace is the JVM wasm
   backend, which dispatch calls the :graal impl. Consumer libraries share
   these parts:
     - The singleton Polyglot Context
     - The WasmContext registry, with one entry for each library
     - The Pointerlike protocol
     - The heap utilities

   This namespace is JVM-only, and the .clj extension is deliberate. squint
   never compiles it, and no npm subpath exports it. The JS wasm backend is a
   separate implementation. Refer to the note at `ccall` for the reason.

   Each library calls create-wasm-context! at boot. It then calls
   set-module! when its module loads. Protocol methods and heap utilities
   resolve the active module through *wasm-context*. Without a binding,
   they use the one registered context instead. Polyglot access serializes
   on the monitor of the Context that owns the touched module. Every
   Context shares one Engine, thus parsed sources and compiled code are
   shared. The default is one Context for the whole JVM, and
   with-graal-lock serializes it. A pool worker instead holds a Context
   from new-polyglot-context!, an unregistered WasmContext record, and a
   module from bootstrap-graal-module! with :polyglot-context.

   Every primitive here accesses a member of an emscripten Module. A
   module from another toolchain is out of scope, by decision.

   The loaded module must carry the members that this namespace touches:
   ccall, getValue, setValue, UTF8ToString, stringToUTF8, _malloc, _free,
   and the HEAP8 to HEAPF64 views. read-heap-array and the heap helpers
   select those views. dispatch reaches ccall through this namespace.

   A consumer arranges these members at link time. The runtime methods and
   heap views come through the emcc-link :exported-runtime-methods. The
   _malloc and _free function exports come through :exported-functions."
  (:require [clojure.tools.logging :as log])
  (:import [com.oracle.truffle.api Truffle]
           [org.graalvm.polyglot Context Engine PolyglotAccess Source Value]
           [org.graalvm.polyglot.proxy ProxyObject ProxyExecutable ProxyArray]
           [java.util.concurrent CompletableFuture]))

(set! *warn-on-reflection* true)

(defn- build-engine
  ^Engine []
  (.build (Engine/newBuilder (into-array String ["js" "wasm"]))))

(defonce ^:private engine-state (delay (build-engine)))

(defn engine
  "The shared polyglot Engine. Every Context here is built on it, thus the
   Contexts share parsed sources and compiled code. Measured 2026-08-21:
   the shared Engine cuts the init of each additional Context from 0.6-1.3
   s to 0.22-0.34 s. The first call constructs it, and namespace load
   never does."
  ^Engine []
  @engine-state)

(defn truffle-runtime-name
  "The name of the active Truffle runtime. \"Interpreted\" means the guest
   runs with no JIT: transforms still work, only slower. An optimizing
   name (for example \"GraalVM CE\") needs a GraalVM JDK, which supplies
   libgraal. Use this to tell the fast path from the fallback; a pass
   count cannot see the difference."
  ^String []
  (.getName (Truffle/getRuntime)))

(defn- build-context
  ^Context []
  (-> (Context/newBuilder (into-array String ["js" "wasm"]))
      (.engine (engine))
      (.allowPolyglotAccess PolyglotAccess/ALL)
      (.option "js.ecmascript-version" "staging")
      (.option "js.esm-eval-returns-exports" "true")
      (.option "js.webassembly" "true")
      (.out System/out)
      (.err System/err)
      (.allowIO true)
      .build))

;; Holds the builder's own Context instance for the life of the JVM. The
;; polyglot API warns on, and may clean up, a Context whose creator
;; instance is collected before close.
(defonce ^:private creator-context-pin (atom nil))

(defonce ^:private context-state
  (delay
    (let [c (build-context)]
      (reset! creator-context-pin c)
     ;; Return the stable "current API" wrapper, the identical instance
     ;; Value.getContext hands back for every value of this Context. The
     ;; builder returns a different wrapper object, and locking works on
     ;; identity. With this derivation, (locking (context) ...) and the
     ;; per-module locks below take one monitor. Measured 2026-08-21:
     ;; .getContext is stable per Context, and distinct across Contexts.
      (.getContext (.asValue c 0)))))

(defn context
  "The shared default GraalVM Polyglot Context. The first call constructs
   it, and namespace load never does. Thus a pure-FFI consumer that loads
   this namespace through dispatch pays no GraalVM startup cost.

   A JVM restart is necessary to create this Context again, because of a
   Graal limitation. Every consumer library on the default Context locks
   on the returned object, through with-graal-lock, to serialize polyglot
   access. A pooled Context from new-polyglot-context! has its own monitor
   and its own lifecycle."
  ^Context []
  @context-state)

(defn new-polyglot-context!
  "Build a fresh Context on the shared Engine, for a pool worker. Nothing
   registers it: the caller owns it and must .close it, for example in a
   workload-pool :destroy. One thread at a time may use it; GraalJS
   permits a thread handoff only between accesses.

   The heap utilities and Pointerlike methods serialize on the monitor of
   the Context that owns the module they touch. Thus a worker that holds
   its own Context never contends with the default Context, or with the
   other workers."
  ^Context []
  (build-context))

(defmacro with-graal-lock
  "Run body while the caller holds the monitor of the shared DEFAULT
   Context. It serializes polyglot access across every consumer library on
   that Context. A pooled Context is out of its scope: the heap utilities
   lock the owning Context of the module they touch.

   The lock forces the lazy construction of the default Context. That is
   safe, because a caller takes the lock only to touch polyglot state. The
   lock is reentrant, because it is a JVM monitor."
  [& body]
  `(locking (context) ~@body))

(defmacro ^:private with-module-lock
  "Run body under the monitor of the Context that owns `module`. Value
   .getContext returns one stable wrapper instance per Context, and for
   the default Context it is the identical instance (context) returns.
   Thus this composes with with-graal-lock on the default Context, and a
   pooled Context serializes on its own monitor."
  [module & body]
  ;; The lock object is shared, per the stability note above; kondo's
  ;; local-lock heuristic cannot see through the accessor.
  `(let [^org.graalvm.polyglot.Value m# ~module]
     #_{:clj-kondo/ignore [:locking-suspicious-lock]}
     (locking (.getContext m#) ~@body)))

(defrecord WasmContext [library-key module-ref])

(defonce ^{:doc "Registry of WasmContext instances, keyed by library-key.
Thus more than one consumer library can coexist in one JVM."}
  contexts
  (atom {}))

(defn create-wasm-context!
  "Spawn a fresh WasmContext for a consumer library. This function is
   idempotent. The same library-key returns the existing context."
  [library-key]
  (or (get @contexts library-key)
      (let [ctx (->WasmContext library-key (atom nil))]
        (swap! contexts assoc library-key ctx)
        ctx)))

(defn lookup-wasm-context
  "Return the WasmContext registered under library-key, or nil."
  [library-key]
  (get @contexts library-key))

(defn set-module!
  "Register the loaded WASM module with a WasmContext. A second call
   overwrites the module, as in the force-graal! and force-ffi! flows."
  [ctx m]
  (reset! (:module-ref ctx) m))

(defn get-module
  "Returns the registered WASM module for the given context. Returns nil
   when its bootstrap did not run yet."
  [ctx]
  @(:module-ref ctx))

(def ^:dynamic *wasm-context*
  "The active WasmContext for the current dynamic scope. Pointerlike
   methods and heap utilities resolve their module through it.
   with-wasm-context binds it. Without a binding, they use the one
   registered context instead."
  nil)

(defmacro with-wasm-context
  "Bind *wasm-context* for body. This scopes the operations onto one
   specific WasmContext, in a JVM with more than one library. A
   single-library consumer can leave it unbound, and then current-module
   uses the one registered context."
  [ctx & body]
  `(binding [*wasm-context* ~ctx]
     ~@body))

(defn- current-module
  "Resolve the active WASM module. Use *wasm-context* first, and the one
   registered context after that. Throws when neither resolves, and also
   when the module is not loaded."
  ^org.graalvm.polyglot.Value []
  (let [ctx (or *wasm-context*
                (let [cs @contexts]
                  (when (= 1 (count cs))
                    (-> cs vals first))))]
    (when (nil? ctx)
      (throw (ex-info "*wasm-context* is unbound and no single default registered context"
                      {:registered-keys (keys @contexts)})))
    (or @(:module-ref ctx)
        (throw (ex-info "WasmContext has no loaded module"
                        {:library-key (:library-key ctx)})))))

(defmacro ^:private with-current-module
  "Resolve the current module, bind it to the symbol in `binding`, and run
   body under the monitor of its owning Context. This is the shared
   prologue of every heap utility that reads the module from the dynamic
   scope rather than from an argument."
  [[binding] & body]
  `(let [~binding (current-module)]
     (with-module-lock ~binding ~@body)))

(declare address-as-trackable-pointer)

(defn- member-fn
  "Reach the member at `path` of `module`. `path` is one member name, or
   a sequence of names for a nested reach like [\"HEAPF64\" \"subarray\"]."
  ^Value [^Value module path]
  (if (sequential? path)
    (reduce (fn [^Value v k] (.getMember v (name k))) module path)
    (.getMember module (name path))))

(defn- coerce-result
  "Read `r` per `as`. The caller holds the module lock: a Value read is
   context access, and it must not interleave with another thread's
   execute."
  [^Value r as]
  (case as
    :value r
    :string (.asString r)
    :int (.asInt r)
    :long (.asLong r)
    :double (.asDouble r)
    :trackable-pointer (address-as-trackable-pointer r)))

(defn module-execute
  "Execute the function member at `path` of `module` with `args`, under
   the module's monitor. `path` is a member name or a sequence of names
   for a nested reach. `as` coerces the result before the monitor
   releases: :value (the default, the raw polyglot Value), :string, :int,
   :long, :double or :trackable-pointer."
  ([module path args] (module-execute module path args :value))
  ([^Value module path args as]
   (with-module-lock module
     (coerce-result (.execute (member-fn module path)
                              (into-array Object args))
                    as))))

(defn graal-execute
  "module-execute against the module of the current dynamic scope. Refer
   to module-execute for `path`, `args` and `as`."
  ([path args] (graal-execute path args :value))
  ([path args as]
   (module-execute (current-module) path args as)))

(defn value-execute
  "Execute the polyglot function Value `f` with `args`, under the monitor
   of the Context that owns it. module-execute for a free-standing fn,
   for example one that module-eval-js returned. `as` coerces as in
   module-execute."
  ([^Value f args] (value-execute f args :value))
  ([^Value f args as]
   (with-module-lock f
     (coerce-result (.execute f (into-array Object args)) as))))

(defn value->int
  "Read `v` as an int, with the interop hint in one place. Use it where
   a polyglot number is not an address, for example a callback argument;
   address-as-int carries the address semantics."
  [^Value v]
  (.asInt v))

(defn value->long
  "Read `v` as a long. The mirror of value->int for i64-range numbers."
  [^Value v]
  (.asLong v))

(defn- build-js-module-source
  "Build a Polyglot Source from a JS module URL. The Source carries the
   ESM MIME type, thus GraalVM honors the import and export syntax."
  [^java.net.URL url]
  (-> (Source/newBuilder "js" (java.io.File. (.toURI url)))
      (.mimeType "application/javascript+module")
      .build))

(defn bootstrap-graal-module!
  "Generic GraalVM bootstrap for a wasm-backed library. This function is
   idempotent. When a module is registered on ctx already, it returns that
   module and does no init again. Returns the loaded module, a Polyglot
   Value.

   ctx  A WasmContext from create-wasm-context!. bootstrap-graal-module!
        calls set-module! on it.

   opts keys:
     :loader-module-url    The URL of the loader module. Refer to the two
                           contracts below.
     :preload-module-urls  Optional URLs. bootstrap-graal-module!
                           evaluates them as ESM before the loader, to
                           help the Graal module resolution.
     :init-opts            A map with string keys. It goes into the
                           ProxyObject that the loader receives. Each
                           value must be polyglot-encoded. js-bytes and
                           js-bytes-map encode binary resources, INTO the
                           same Context that :polyglot-context names.
     :polyglot-context     Optional target Context, for a pool worker
                           with a Context from new-polyglot-context!.
                           The default is the shared default Context. A
                           pool worker pairs this with an unregistered
                           record from (->WasmContext lib-key (atom nil)),
                           so the global registry stays single-context.

   Loader contracts, in the order that this fn looks for them:

     load(opts) -> a module, or a promise of a module. This contract is
     preferred. bootstrap-graal-module! owns the bridge from callback to
     future. Thus the loader is one async function. It returns what it
     loaded, and it throws what went wrong. It receives :init-opts
     unchanged.

     initialize(opts), with a call to opts.onSuccess(module) or
     opts.onError(err). This is the older shape, and it still works. It
     receives :init-opts, and also the injected onSuccess and onError
     keys. A caller must not supply those two keys.

   With each contract, the load is asynchronous inside JS. It is
   synchronous to the caller here, which blocks on a CompletableFuture.

   That works because GraalJS drains its promise job queue when JS
   execution returns to this embedder. Thus a chain of awaits over values
   that are available already settles before .execute returns. A loader
   that waits on a timer or on real I/O would not settle. That is the
   deeper reason for the two rules below.

   What a loader module can do and cannot do. GraalVM causes all of it,
   and no one library does:

     No package resolution. The module loads as a bare ESM Source. Thus a
     relative import of a sibling resolves, and a bare specifier does not.
     A loader therefore cannot import the shipped .mjs helpers of this
     package. It keeps its own copy of each helper that it must have.

     No URL global. The emscripten findWasmBinary uses
     `new URL(name, import.meta.url)` when the module arguments set no
     locateFile. That reference throws here, even with a wasmBinary. A
     simple `locateFile: (path) => path` prevents the construction of the
     URL. Nothing fetches the returned path, because the bytes are
     available already.

     No setTimeout. The emscripten run() wraps doRun() in a timer only
     when Module.setStatus is present. Thus a loader must leave setStatus
     unset. Then run() calls doRun() directly, and onRuntimeInitialized
     fires with no timer.

   bootstrap-graal-module! does the caching, and the loader does not. The
   module-ref guard above calls a loader one time at most for each
   WasmContext. Thus nothing can reach a module cache inside the loader."
  [ctx {:keys [loader-module-url preload-module-urls init-opts polyglot-context]}]
  ;; :module-ref is the shared atom field of the WasmContext record, and
  ;; thus a stable monitor for each context. clj-kondo cannot see through
  ;; the record keyword lookup to know that the atom is shared. Thus it
  ;; flags the local-lock heuristic.
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:module-ref ctx)
    (or @(:module-ref ctx)
        (let [^Context pctx (or polyglot-context (context))]
          (when (nil? loader-module-url)
            (throw (ex-info "bootstrap-graal-module! requires :loader-module-url"
                            {:ctx (:library-key ctx)})))
          (doseq [url preload-module-urls]
            (when url
              (locking pctx
                (let [src (build-js-module-source url)]
                  (log/info "Pre-loading JS module" (str url))
                  (.eval pctx src)))))
          (let [loader-module (locking pctx
                                (let [src (build-js-module-source loader-module-url)]
                                  (log/info "Loading JS loader module" (str loader-module-url))
                                  (.eval pctx src)))
                init-future (CompletableFuture.)
                settle! (fn [m]
                          (log/info "Module init success callback fired for"
                                    (:library-key ctx))
                          (when m
                            (set-module! ctx m)
                            (.complete init-future m)))
                on-success (reify ProxyExecutable
                             (execute [_ args]
                               (settle! (when (pos? (alength args)) (aget args 0)))
                               nil))
                on-error (reify ProxyExecutable
                           (execute [_ args]
                             (let [err (if (pos? (alength args)) (aget args 0) "Unknown error")]
                               (log/error err "Module init error callback fired for"
                                          (:library-key ctx))
                               (.completeExceptionally init-future
                                                       (ex-info "Module init failed"
                                                                {:library-key (:library-key ctx)
                                                                 :error       err}))
                               nil)))
                ^org.graalvm.polyglot.Value loader loader-module
                load-fn (.getMember loader "load")
                init-fn (.getMember loader "initialize")]
            (cond
              (and load-fn (.canExecute load-fn))
              ;; The load contract. The loader returns what it loaded.
              ;; This code bridges the returned promise onto the future
              ;; that the caller blocks on. A loader that is not async
              ;; returns the module itself, which has no `then`. Thus
              ;; complete the future directly.
              (let [^Value returned (locking pctx
                                      (log/info "Executing 'load' for" (:library-key ctx))
                                      (.execute load-fn (into-array Object [(ProxyObject/fromMap (or init-opts {}))])))]
                (if (.canInvokeMember returned "then")
                  (locking pctx
                    (.invokeMember returned "then" (into-array Object [on-success on-error])))
                  (settle! returned)))

              (and init-fn (.canExecute init-fn))
              (locking pctx
                (log/info "Executing 'initialize' for" (:library-key ctx))
                (.execute init-fn (into-array Object
                                              [(ProxyObject/fromMap
                                                (assoc (or init-opts {})
                                                       "onSuccess" on-success
                                                       "onError"   on-error))])))

              :else
              (throw (ex-info "Loader module exports neither load nor initialize"
                              {:library-key       (:library-key ctx)
                               :loader-module-url (str loader-module-url)})))
            (log/info "Awaiting initialization completion for" (:library-key ctx))
            (.get init-future)
            (log/info "Module ready for" (:library-key ctx))
            @(:module-ref ctx))))))

(defn eval-js
  "Evaluate a JS source snippet. The 1- and 2-arity forms target the
   shared default Context, under its monitor. The 3-arity form targets the
   given Context, under that Context's monitor, for a pool worker. One
   example is the construction of a JS Array for a bulk transfer."
  ([source]
   (eval-js source "src.js"))
  ([source js-name]
   (eval-js (context) source js-name))
  ([^Context ctx ^String source ^String js-name]
   (locking ctx
     (.eval ctx
            (.build
             (Source/newBuilder "js" ^String source js-name))))))

(defn module-eval-js
  "Evaluate a JS source snippet in the Context that owns `module`, under
   that Context's monitor. A Value is unusable outside the Context that
   built it, so a helper fn that a delay shares across Contexts breaks on
   a pool worker; evaluate per Context instead. The Engine source cache
   makes the re-eval cheap."
  [^Value module ^String source ^String js-name]
  (eval-js (.getContext module) source js-name))

(defn- byte-array-proxy
  "Read-only array view over a Java byte array. It widens each signed byte
   to the range 0 to 255 as JS reads it. This is a view, thus it copies
   nothing on this side.

   The mask serves the semantics of this view, and not the copy. A store
   into the Uint8Array that js-bytes builds wraps modulo 256, and it lands
   on the same value in each case. Any other reader of this view gets the
   unsigned byte that it asks for."
  ^ProxyArray [^bytes b]
  (reify ProxyArray
    (get [_ index] (bit-and (aget b (int index)) 0xff))
    (set [_ _index _value]
      (throw (UnsupportedOperationException. "byte-array-proxy is read-only")))
    (getSize [_] (alength b))))

;; One Source instance, evaluated per call in the target Context. The
;; Engine source cache makes the re-eval cheap, and it is noise next to
;; the per-byte copy loop the returned function runs.
(def ^:private bytes-widener-source
  (.build (Source/newBuilder "js"
                             "(src) => { const n = src.length; const a = new Uint8Array(n); for (let i = 0; i < n; i++) a[i] = src[i]; return a; }"
                             "clj-native-bytes.js")))

(defn js-bytes
  "Copy a Java byte array into a JS Uint8Array inside a polyglot Context.
   The 1-arity form targets the shared default Context. The 2-arity form
   targets the given Context; a Uint8Array is unusable outside the Context
   that built it, so a pool worker must encode into its own.

   Binary resources reach a loader module through this function. A loader
   would otherwise undo an unchanged array itself. Java bytes are signed,
   thus every byte above 127 arrives negative, and the JS side must widen
   it. The work here gives a loader a real Uint8Array, and the loader can
   use .buffer directly."
  ([^bytes b] (js-bytes (context) b))
  ([^Context ctx ^bytes b]
   (locking ctx
     (.execute ^Value (.eval ctx bytes-widener-source)
               (into-array Object [(byte-array-proxy b)])))))

(defn js-bytes-map
  "Encode a name -> byte-array map as a JS object of Uint8Arrays, for a
   loader that installs a set of sidecar files. The 2-arity form encodes
   into the given Context, as with js-bytes."
  ([m] (js-bytes-map (context) m))
  ([^Context ctx m]
   (ProxyObject/fromMap
    (into {} (map (fn [[k v]] [(name k) (js-bytes ctx v)])) m))))

(defn heap-write-bytes!
  "Copy `b` into the HEAPU8 of `module`, from wasm address `ptr`. Returns
   the number of written bytes.

   Each byte crosses the polyglot boundary on its own, and this function
   widens it to the range 0 to 255. A host byte array is not a JS typed
   array, and the heap view does not accept one.

   heap-write-bytes! reads HEAPU8 here, and the caller does not read it.
   That is on purpose. An allocation can grow the memory, which swaps the
   buffer below and detaches every earlier view."
  ^long [^Value module ^long ptr ^bytes b]
  (with-module-lock module
    (let [heapu8 (.getMember module "HEAPU8")
          n      (alength b)]
      (dotimes [i n]
        (.setArrayElement heapu8 (+ ptr i) (bit-and (aget b i) 0xFF)))
      n)))

(defn heap-write-doubles!
  "Copy the doubles in `xs` into the HEAPF64 of `module`, from wasm
   address `ptr`. `ptr` must be 8-byte aligned. Returns the number of
   written doubles. The 2-arity form resolves the module of the current
   dynamic scope, as read-heap-array does.

   Each double crosses the polyglot boundary on its own setArrayElement,
   under one lock for the whole loop. heap-write-doubles! reads HEAPF64
   here, and the caller does not read it, for the same detach reason as
   heap-write-bytes!."
  (^long [ptr xs]
   (heap-write-doubles! (current-module) ptr xs))
  (^long [^Value module ^long ptr ^doubles xs]
   (with-module-lock module
     (let [heapf64 (.getMember module "HEAPF64")
           b (bit-shift-right ptr 3)
           n (alength xs)]
       (dotimes [i n]
         (.setArrayElement heapf64 (+ b i) (aget xs i)))
       n))))

(defn utf8->string
  "Read the NUL-terminated UTF-8 string at wasm address `ptr`, through the
   UTF8ToString export of `module`."
  ^String [^Value module ptr]
  (module-execute module "UTF8ToString" [ptr] :string))

;; Why a second ccall implementation exists.
;;
;; Two backends speak ccall, because they share the emscripten artifact and
;; not the host. This one serves the JVM. handler_heap.mjs/ccallMethod serves
;; the JS host. The pair is a decision, and not an oversight.
;;
;; The JVM could instead load the squint-compiled clj-native (pool.mjs and
;; dispatch.mjs) into GraalJS and reach the module through that stack. This
;; namespace ccalls the module directly, to avoid the second driver layer and
;; the JS-to-host marshaling on every call. That is the chosen half of the
;; decision, and it is what produces this duplicate pair.
;;
;; A separate constraint is forced, and not chosen: a loader module that a
;; polyglot Context evaluates cannot import a bare specifier at all, so it
;; cannot reach the shipped .mjs helpers. bootstrap-graal-module! documents
;; that one. Do not read the forced constraint as the reason for this pair.
;;
;; The same split repeats for the heap reads, the string-array walk, the
;; struct read and the synchronous HTTP transport. The README lists the whole
;; set under "Two hosts, one artifact".
(defn ccall
  "Call the exported C function `c-fn-name` through the ccall export of
   `module`. `rettype` and every entry of `argtypes` name an emscripten
   ccall type, as a keyword or as a string. Returns the raw polyglot
   result, and the caller coerces it.

   The type list and the argument list MUST arrive as ProxyArray. ccall
   indexes the two as JS arrays. A host Object array arrives with no
   JS-array indexing, thus the arguments never reach the C function. The
   call still returns, and that makes the mistake expensive. A
   registration function reports success while it receives nothing."
  [^Value module c-fn-name rettype argtypes args]
  (module-execute module "ccall"
                  [c-fn-name
                   (name rettype)
                   (ProxyArray/fromArray (object-array (map name argtypes)))
                   (ProxyArray/fromArray (object-array args))]))

(defn put-js-globals!
  "Publish every value of `m` on globalThis, under its key. The 1-arity
   form targets the shared default Context; the 2-arity form targets the
   given Context, for a pool worker.

   This function publishes a host callback that C code must reach.
   addFunction is the alternative, and it corrupts the GraalVM WASM
   function table. The C stub of the library reads the global at call
   time, and that completes the bridge."
  ([m] (put-js-globals! (context) m))
  ([^Context ctx m]
   (locking ctx
     (let [bindings (.getBindings ctx "js")]
       (doseq [[k v] m]
         (.putMember bindings (name k) v))))))

(defprotocol Pointerlike
  (address-as-int [this])
  (address-as-string [this])
  (address-as-polyglot-value [this])
  (address-as-trackable-pointer [this])
  (get-value [this type])
  (pointer->string [this])
  (string-array-pointer->strs [this]))

(defrecord TrackablePointer [address]
  Pointerlike
  (address-as-int [this] (address-as-int (:address this)))
  (address-as-string [this] (address-as-string (:address this)))
  (address-as-polyglot-value [this] (address-as-polyglot-value (:address this)))
  (address-as-trackable-pointer [this] this)
  (get-value [this type] (get-value (:address this) type))
  (pointer->string [this] (pointer->string (:address this)))
  (string-array-pointer->strs [this] (string-array-pointer->strs (:address this))))

(def ^:dynamic *runtime-log-level*
  "Optional log level (:debug, :info) for verbose Pointerlike tracing."
  nil)

(extend-protocol Pointerlike
  org.graalvm.polyglot.Value
  (address-as-int [this] (.asInt this))
  (address-as-string [this] (.asString this))
  (address-as-polyglot-value [this] this)
  (address-as-trackable-pointer [this]
    (let [addr (if (.isNumber this) (.asLong this) 0)]
      (when (not (.isNumber this))
        (log/error "Polyglot Value is not a number when creating TrackablePointer:" this))
      (->TrackablePointer addr)))
  (get-value [this type]
    (graal-execute "getValue" [this type]))
  (pointer->string [this]
    (utf8->string (current-module) this))
  (string-array-pointer->strs [this]
    (loop [addr this
           result-strings []
           idx 0]
      (when *runtime-log-level*
        (log/log *runtime-log-level* (str "Graal: string-array-pointer->strs - Loop iteration " idx ", reading from address: " (address-as-int addr))))
      (let [string-addr-polyglot (get-value (address-as-int addr) "*")
            string-addr-int (address-as-int string-addr-polyglot)]
        (when *runtime-log-level*
          (log/log *runtime-log-level* (str "Graal: string-array-pointer->strs - Pointer at " (address-as-int addr) " points to string at: " string-addr-int)))
        (if (zero? string-addr-int)
          (do (when *runtime-log-level*
                (log/log *runtime-log-level* (str "Graal: string-array-pointer->strs - Found null terminator, returning: " result-strings)))
              result-strings)
          (let [current-str (pointer->string string-addr-polyglot)]
            (when *runtime-log-level*
              (log/log *runtime-log-level* (str "Graal: string-array-pointer->strs - Read string: \"" current-str "\"")))
            (recur (address-as-polyglot-value (+ (address-as-int addr) 4))
                   (conj result-strings current-str)
                   (inc idx))))))))

;; A host scalar carries an address and nothing else. Thus five of the seven
;; methods are the same for every such type: wrap the scalar as a polyglot
;; Value, then delegate. A type states only how it becomes an int and a
;; string. extend-protocol expands to exactly this extend call, thus the
;; shared map makes no difference to dispatch.
;;
;; The wrap targets the Context of the CURRENT module, not the default
;; Context. A Value is unusable outside the Context that built it, so a
;; wrap through the default Context throws on every pooled-context call.
(def ^:private scalar-address-impl
  {:address-as-polyglot-value (fn [this]
                                (with-current-module [m]
                                  (.asValue (.getContext m) this)))
   :address-as-trackable-pointer (fn [this] (->TrackablePointer (address-as-int this)))
   :get-value (fn [this type] (get-value (address-as-polyglot-value this) type))
   :pointer->string (fn [this] (pointer->string (address-as-polyglot-value this)))
   :string-array-pointer->strs (fn [this]
                                 (string-array-pointer->strs
                                  (address-as-polyglot-value this)))})

(extend java.lang.String
  Pointerlike
  (assoc scalar-address-impl
         :address-as-int (fn [this] (Integer/parseInt this))
         :address-as-string (fn [this] this)))

(extend java.lang.Long
  Pointerlike
  (assoc scalar-address-impl
         :address-as-int (fn [this] (int this))
         :address-as-string (fn [this] (str this))))

(extend java.lang.Integer
  Pointerlike
  (assoc scalar-address-impl
         :address-as-int (fn [this] this)
         :address-as-string (fn [this] (str this))))

(defn malloc
  "Allocate `b` bytes on the WASM heap. Returns a TrackablePointer. The
   caller must call free-on-heap, or register a destroy hook."
  [b]
  (graal-execute "_malloc" [b] :trackable-pointer))

(defn heapf64
  "Return a polyglot subarray view over the HEAPF64 region. The view
   starts at the given offset, in 8-byte units, and it covers `n`
   doubles."
  [offset n]
  (graal-execute ["HEAPF64" "subarray"] [offset (+ offset n)]))

(defn allocate-string-on-heap
  "Allocate a UTF-8 string on the WASM heap. Returns a TrackablePointer to
   the start. Returns nil for a nil input."
  [^String s]
  (when s
    (let [len (+ 1 (alength (.getBytes s "UTF-8")))
          addr (malloc len)]
      (graal-execute "stringToUTF8" [s (address-as-polyglot-value addr) len])
      addr)))

(defn- write-pointer-slots!
  "Write the wasm32 address of each Pointerlike in `ptrs` into consecutive
   4-byte slots. The slots are in the heap of the active module, and they
   start at raw address `base`. write-pointer-slots! uses setValue with
   type \"*\". Returns `base`.

   The caller supplies the module and the lock scope, through
   with-current-module. This is the shared core that writes slots for
   pointers->wasm-array, a bare table, and for
   string-list-to-native-array, a NULL-terminated char**."
  ^long [^Value module ^long base ptrs]
  (let [set-value (.getMember module "setValue")]
    (dotimes [i (count ptrs)]
      (.execute set-value
                (into-array Object [(+ base (* i 4))
                                    (address-as-int (nth ptrs i))
                                    "*"])))
    base))

(defn string-list-to-native-array
  "Allocate a NULL-terminated char** on the WASM heap from a sequence of
   strings. Returns a Polyglot.Value at the start of the pointer array. A
   consumer must free the array, and also each string pointer inside it."
  [s-list]
  (if (empty? s-list)
    (with-current-module [m] (.asValue (.getContext m) 0))
    (let [_ (when (some nil? s-list)
              (throw (ex-info "string-list-to-native-array: nil element in s-list (char** cannot represent a null string); validate before calling"
                              {:s-list s-list})))
          string-pointers (mapv allocate-string-on-heap s-list)
          num-strings (count string-pointers)
          array-of-pointers-size (* (inc num-strings) 4)
          array-of-pointers-addr (address-as-polyglot-value (malloc array-of-pointers-size))
          base (long (address-as-int array-of-pointers-addr))]
      (with-current-module [m]
        (write-pointer-slots! m base string-pointers)
        ;; The NULL terminator lets a reader walk the char** to its end.
        (.execute (.getMember m "setValue")
                  (into-array Object [(+ base (* num-strings 4)) 0 "*"]))
        array-of-pointers-addr))))

(defn free-on-heap
  "Free a WASM heap pointer from an earlier allocation. A nil pointer is
   safe."
  [ptr]
  (when ptr
    (graal-execute "_free" [(address-as-polyglot-value ptr)])))

(defn pointers->wasm-array
  "Pack the addresses of `ptrs`, which are Pointerlike, into a fresh
   wasm-heap pointer table. Each slot is 4 bytes on wasm32, and setValue
   \"*\" writes it. Returns a TrackablePointer, and the caller frees it
   with free-on-heap.

   This is the WASM mirror of ffi-mem/ptrs->native-array. There is no
   trailing NULL, because this is a fixed-length argument array. For a
   NULL-terminated char**, use string-list-to-native-array."
  [ptrs]
  (let [arr (malloc (* 4 (count ptrs)))]
    (with-current-module [m]
      (write-pointer-slots! m (long (address-as-int arr)) ptrs))
    arr))

(defn read-heap-array
  "Read `n` elements from a typed HEAP view of the active module, from
   pointer `ptr`. `heap-type` selects the view. It is one of :u8, :i8,
   :u16, :i16, :u32, :i32, :f32 and :f64. The result is the natural Java
   primitive array for that view.

   The unsigned 16-bit and 32-bit views widen to short[] and int[],
   because the JVM has no unsigned primitives. A consumer interprets the
   bit pattern. The dtype codes of a library map to `heap-type` at the
   caller. This is the WASM read-side mirror of a typed native-buffer
   read.

   The eight-arm `case` stays written out. A data-driven table costs the
   static primitive-array types that `*warn-on-reflection*` needs. Do not
   revisit this without a new argument."
  [ptr n heap-type]
  (with-current-module [module]
    (let [base (long (address-as-int ptr))]
      (case heap-type
        :u8  (let [h ^Value (.getMember module "HEAPU8") out (byte-array n)]
               (dotimes [i n] (aset out i (unchecked-byte (.asInt (.getArrayElement h (+ base i))))))
               out)
        :i8  (let [h ^Value (.getMember module "HEAP8") out (byte-array n)]
               (dotimes [i n] (aset out i (unchecked-byte (.asInt (.getArrayElement h (+ base i))))))
               out)
        :u16 (let [h ^Value (.getMember module "HEAPU16") b (bit-shift-right base 1) out (short-array n)]
               (dotimes [i n] (aset out i (unchecked-short (.asInt (.getArrayElement h (+ b i))))))
               out)
        :i16 (let [h ^Value (.getMember module "HEAP16") b (bit-shift-right base 1) out (short-array n)]
               (dotimes [i n] (aset out i (unchecked-short (.asInt (.getArrayElement h (+ b i))))))
               out)
        :u32 (let [h ^Value (.getMember module "HEAPU32") b (bit-shift-right base 2) out (int-array n)]
               (dotimes [i n] (aset out i (unchecked-int (.asLong (.getArrayElement h (+ b i))))))
               out)
        :i32 (let [h ^Value (.getMember module "HEAP32") b (bit-shift-right base 2) out (int-array n)]
               (dotimes [i n] (aset out i (.asInt (.getArrayElement h (+ b i)))))
               out)
        :f32 (let [h ^Value (.getMember module "HEAPF32") b (bit-shift-right base 2) out (float-array n)]
               (dotimes [i n] (aset out i (float (.asDouble (.getArrayElement h (+ b i))))))
               out)
        :f64 (let [h ^Value (.getMember module "HEAPF64") b (bit-shift-right base 3) out (double-array n)]
               (dotimes [i n] (aset out i (.asDouble (.getArrayElement h (+ b i)))))
               out)
        (throw (ex-info (str "read-heap-array unsupported heap-type " heap-type)
                        {:heap-type heap-type}))))))

(defn read-struct
  "Read a C struct at `struct-addr` from the WASM heap of the active
   module, into a map. `fields` is a vector of [key type wasm-offset]. The
   type is one of :string, :int, :double and :boolean. This is the WASM
   mirror of a native-buffer struct read.

   A :string field at a null address gives nil."
  [struct-addr fields]
  (with-current-module [module]
    (let [get-value (.getMember module "getValue")
          utf8 (.getMember module "UTF8ToString")
          gv (fn [ptr t] (.asInt (.execute get-value (object-array [ptr t]))))]
      (persistent!
       (reduce (fn [m [kw ftype off]]
                 (assoc! m kw
                         (case ftype
                           :string (let [a (gv (+ struct-addr off) "*")]
                                     (when-not (zero? a)
                                       (.asString (.execute utf8 (object-array [a])))))
                           :int (gv (+ struct-addr off) "i32")
                           :double (.asDouble (.execute get-value
                                                        (object-array [(+ struct-addr off) "double"])))
                           :boolean (not= 0 (gv (+ struct-addr off) "i32")))))
               (transient {})
               fields)))))
