;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.graal-wasm
  "The JVM wasm backend on GraalVM polyglot, which dispatch calls as the
   :graal impl: the shared Engine and Contexts, the WasmContext registry,
   TrackablePointer and the heap utilities.

   Each library calls create-wasm-context! at boot, then
   bootstrap-graal-module! to load its module. Heap calls use the module
   of *wasm-context*, else of the one registered context. With more than
   one library loaded, wrap heap calls in with-library-context. Polyglot
   access locks the monitor of the Context that owns the module.

   The module must be an emscripten Module that exports ccall, getValue,
   setValue, UTF8ToString, stringToUTF8 and the HEAP8 to HEAPF64 views
   (:exported-runtime-methods), and _malloc and _free
   (:exported-functions)."
  (:require [clojure.tools.logging :as log])
  (:import [com.oracle.truffle.api Truffle]
           [org.graalvm.polyglot Context Engine PolyglotAccess Source Value]
           [org.graalvm.polyglot.proxy ProxyObject ProxyExecutable ProxyArray]
           [java.util.concurrent CompletableFuture]))

(set! *warn-on-reflection* true)

(defn- build-engine
  ^Engine []
  (.build (Engine/newBuilder (into-array String ["js" "wasm"]))))

;; Every Context shares this Engine, and with it parsed sources and
;; compiled code.
(defonce ^:private engine-state (delay (build-engine)))

(defn truffle-runtime-name
  "The active Truffle runtime name. \"Interpreted\" means no JIT: the guest
   runs, only slower. An optimizing runtime needs a GraalVM JDK."
  ^String []
  (.getName (Truffle/getRuntime)))

(defn- fill-random!
  "Fill the bytes under the typed array `view` from `rng`. Returns `view`."
  ^Value [^java.security.SecureRandom rng ^Value view]
  (let [buf (.getMember view "buffer")
        off (.asLong (.getMember view "byteOffset"))
        n   (.asLong (.getMember view "byteLength"))]
    (loop [i 0]
      (cond
        (<= (+ i 8) n) (do (.writeBufferLong buf java.nio.ByteOrder/LITTLE_ENDIAN (+ off i) (.nextLong rng))
                           (recur (+ i 8)))
        (< i n)        (do (.writeBufferByte buf (+ off i) (unchecked-byte (.nextInt rng)))
                           (recur (inc i)))))
    view))

(defn- install-crypto!
  "Give `ctx` a crypto.getRandomValues, which GraalJS lacks. An emscripten
   module reads /dev/urandom through it."
  ^Context [^Context ctx]
  (let [bindings (.getBindings ctx "js")]
    (when-not (.hasMember bindings "crypto")
      (let [rng (java.security.SecureRandom.)
            get-random-values (reify ProxyExecutable
                                (execute [_ args]
                                  (fill-random! rng (aget ^"[Lorg.graalvm.polyglot.Value;" args 0))))]
        (.putMember bindings "crypto"
                    (ProxyObject/fromMap {"getRandomValues" get-random-values}))))
    ctx))

(defn- build-context
  ^Context []
  (-> (Context/newBuilder (into-array String ["js" "wasm"]))
      (.engine ^Engine @engine-state)
      (.allowPolyglotAccess PolyglotAccess/ALL)
      (.option "js.ecmascript-version" "staging")
      (.option "js.esm-eval-returns-exports" "true")
      (.option "js.webassembly" "true")
      (.out System/out)
      (.err System/err)
      (.allowIO true)
      .build
      install-crypto!))

;; The polyglot API may clean up a Context whose creator instance is
;; collected before close.
(defonce ^:private builder-context (atom nil))

(defonce ^:private context-state
  (delay
    (let [c (build-context)]
      (reset! builder-context c)
     ;; The builder returns a different wrapper than Value.getContext does.
     ;; Use the getContext one, so (context) and the module locks share one
     ;; monitor.
      (.getContext (.asValue c 0)))))

(defn context
  "The shared default Context. The first call builds it, so a pure-FFI
   consumer pays no GraalVM startup. Only a JVM restart can build it again.
   with-graal-lock locks on it."
  ^Context []
  @context-state)

(defn new-polyglot-context!
  "A new unregistered Context on the shared Engine, for a pool worker. The
   caller owns it and must .close it. Use it from one thread at a time;
   GraalJS allows a thread handoff only between accesses."
  ^Context []
  (build-context))

(defmacro with-graal-lock
  "Run body under the monitor of the default Context, which it builds when
   needed. A pooled Context has its own monitor. Reentrant."
  [& body]
  `(locking (context) ~@body))

(defmacro ^:private with-module-lock
  "Run body under the monitor of the Context that owns `module`. On the
   default Context this is the with-graal-lock monitor."
  [module & body]
  ;; kondo cannot see that .getContext returns a shared object.
  `(let [^org.graalvm.polyglot.Value m# ~module]
     #_{:clj-kondo/ignore [:locking-suspicious-lock]}
     (locking (.getContext m#) ~@body)))

(defrecord WasmContext [library-key module-ref])

(defonce ^{:doc "WasmContext registry, keyed by library-key."}
  contexts
  (atom {}))

(defn create-wasm-context!
  "The WasmContext of `library-key`. The first call creates and registers
   it."
  [library-key]
  (get (swap! contexts update library-key #(or % (->WasmContext library-key (atom nil))))
       library-key))

(defn get-module
  "The module registered on `ctx`, or nil before bootstrap."
  [ctx]
  @(:module-ref ctx))

(def ^:dynamic *wasm-context*
  "The WasmContext whose module the heap utilities use. Unbound, they use
   the one registered context."
  nil)

(defmacro with-wasm-context
  "Run body with *wasm-context* bound to `ctx`."
  [ctx & body]
  `(binding [*wasm-context* ~ctx]
     ~@body))

(defn library-context
  "The bound *wasm-context*, else the WasmContext of `library-key`. Throws
   when neither exists. A binding wins even when it belongs to another
   library, so do not call one library inside another's
   with-library-context."
  [library-key]
  (or *wasm-context*
      (get @contexts library-key)
      (throw (ex-info "No WasmContext is registered for this library"
                      {:library-key library-key}))))

(defmacro with-library-context
  "Run body with *wasm-context* bound to (library-context library-key)."
  [library-key & body]
  `(binding [*wasm-context* (library-context ~library-key)]
     ~@body))

(defn- current-module
  "The module of *wasm-context*, else of the one registered context.
   Throws when neither exists or the module is not loaded."
  ^org.graalvm.polyglot.Value []
  (let [ctx (or *wasm-context*
                (let [cs @contexts]
                  (when (= 1 (count cs))
                    (-> cs vals first))))]
    (when (nil? ctx)
      (throw (ex-info (str "*wasm-context* is unbound and no single default registered context. "
                           "Wrap the heap call in with-library-context.")
                      {:registered-keys (keys @contexts)})))
    (or @(:module-ref ctx)
        (throw (ex-info "WasmContext has no loaded module"
                        {:library-key (:library-key ctx)})))))

(defmacro ^:private with-current-module
  "Bind the current module to `binding` and run body under the monitor of
   its Context."
  [[binding] & body]
  `(let [~binding (current-module)]
     (with-module-lock ~binding ~@body)))

(declare address-as-int address-as-trackable-pointer)

(defn- coerce-result
  "Read `r` per `as`. Call it under the module lock, because a Value read
   is Context access."
  [^Value r as]
  (case as
    :value r
    :string (.asString r)
    :int (.asInt r)
    :trackable-pointer (address-as-trackable-pointer r)))

(defn module-execute
  "Execute the member `path` of `module`, a name, with `args` under the
   module's monitor. `as` coerces the result inside the lock: :value
   (default), :string, :int or :trackable-pointer."
  ([module path args] (module-execute module path args :value))
  ([^Value module path args as]
   (with-module-lock module
     (coerce-result (.execute (.getMember module (name path))
                              (into-array Object args))
                    as))))

(defn- graal-execute
  "module-execute on the current module."
  ([path args] (graal-execute path args :value))
  ([path args as]
   (module-execute (current-module) path args as)))

(defn value-execute
  "Execute the polyglot fn `f` with `args` under the monitor of its
   Context. `as` is as in module-execute."
  ([^Value f args] (value-execute f args :value))
  ([^Value f args as]
   (with-module-lock f
     (coerce-result (.execute f (into-array Object args)) as))))

(defn value->int
  "(.asInt v) with the Value hint, so a caller needs no import."
  [^Value v]
  (.asInt v))

(defn value->long
  "(.asLong v) with the Value hint, so a caller needs no import."
  [^Value v]
  (.asLong v))

(defonce ^:private module-copy-dir
  (delay (doto (.toFile (java.nio.file.Files/createTempDirectory
                         "clj-native-modules"
                         (make-array java.nio.file.attribute.FileAttribute 0)))
           (.deleteOnExit))))

(defn- module-file
  "The file of the JS module at `url`. A Source needs a file to resolve
   relative imports, so a jar: URL is copied once per JVM, into a temp dir
   per URL directory."
  ^java.io.File [^java.net.URL url]
  (if (= "file" (.getProtocol url))
    (java.io.File. (.toURI url))
    (let [s   (str url)
          i   (.lastIndexOf s "/")
          dir (java.io.File. ^java.io.File @module-copy-dir
                             (str (java.util.UUID/nameUUIDFromBytes (.getBytes (subs s 0 i) "UTF-8"))))
          f   (java.io.File. dir (subs s (inc i)))]
      ;; Pool workers bootstrap in parallel. A second copy would replace a
      ;; file that another Context is importing.
      (locking module-copy-dir
        (when-not (.exists f)
          (.mkdirs dir)
          (.deleteOnExit dir)
          ;; Each later bootstrap in this JVM would import a partial copy.
          (try
            (with-open [in (.openStream url)]
              (java.nio.file.Files/copy in (.toPath f)
                                        ^"[Ljava.nio.file.CopyOption;" (make-array java.nio.file.CopyOption 0)))
            (catch Throwable t
              (.delete f)
              (throw t)))
          (.deleteOnExit f)))
      f)))

(defn- eval-module!
  "Evaluate the ESM module at `url` in `pctx`. The caller holds the monitor
   of `pctx`."
  ^Value [^Context pctx url]
  (.eval pctx (-> (Source/newBuilder "js" (module-file url))
                  (.mimeType "application/javascript+module")
                  .build)))

(defn- run-loader!
  "Call the load or initialize export of `loader`. The caller holds the
   monitor of its Context. Returns a CompletableFuture of the module, which
   fails when the loader gives none or does not settle."
  ^CompletableFuture [^Value loader init-opts library-key]
  (let [result     (CompletableFuture.)
        fail!      #(.completeExceptionally result (ex-info "Module init failed"
                                                            {:library-key library-key :error %}))
        done!      (fn [^Value m]
                     (if (or (nil? m) (.isNull m))
                       (fail! "the loader gave no module")
                       (.complete result m)))
        on-success (reify ProxyExecutable
                     (execute [_ args]
                       (done! (when (pos? (alength args)) (aget args 0)))
                       nil))
        on-error   (reify ProxyExecutable
                     (execute [_ args]
                       (fail! (if (pos? (alength args)) (aget args 0) "Unknown error"))
                       nil))
        load-fn    (.getMember loader "load")
        init-fn    (.getMember loader "initialize")]
    (cond
      (and load-fn (.canExecute load-fn))
      ;; A loader that is not async returns the module, which has no `then`.
      (let [returned (.execute load-fn (into-array Object [(ProxyObject/fromMap (or init-opts {}))]))]
        (if (.canInvokeMember returned "then")
          (.invokeMember returned "then" (into-array Object [on-success on-error]))
          (done! returned)))

      (and init-fn (.canExecute init-fn))
      (.execute init-fn (into-array Object
                                    [(ProxyObject/fromMap
                                      (assoc (or init-opts {})
                                             "onSuccess" on-success
                                             "onError"   on-error))]))

      :else
      (throw (ex-info "Loader module exports neither load nor initialize"
                      {:library-key library-key})))
    ;; GraalJS runs promise jobs before JS returns to the host, so a load
    ;; that is still pending waits on a timer or I/O that never comes.
    (when-not (.isDone result)
      (fail! "the loader did not settle"))
    result))

(defn bootstrap-graal-module!
  "Load the wasm module of `ctx`, a WasmContext, store it on `ctx`, and
   return the module Value. Blocks until the load settles. When `ctx` has
   a module already, returns it and calls no loader.

   opts:
     :loader-module-url    Required. The loader ESM module.
     :preload-module-urls  ESM modules to evaluate before the loader.
     :init-opts            String-keyed map for the loader. Encode bytes
                           with js-bytes into the target Context.
     :polyglot-context     A Context from new-polyglot-context!, for a pool
                           worker, with an unregistered
                           (->WasmContext lib-key (atom nil)). Default:
                           (context).

   The loader exports one of:
     load(opts)        Returns the module, or a promise of it.
     initialize(opts)  Calls opts.onSuccess(module) or opts.onError(err).
                       Do not put those keys in :init-opts.

   GraalVM limits the loader:
     - No package resolution. A bare import specifier fails, so the loader
       cannot import the .mjs helpers of this package. A jar: module loads
       from a temp copy: list each sibling it imports in
       :preload-module-urls.
     - No URL global. Set locateFile, such as (path) => path, so emscripten
       does not call new URL.
     - No setTimeout. Leave Module.setStatus unset, so run() calls doRun()
       with no timer. GraalJS settles promises only when JS returns to the
       host, so a load that waits on a timer or real I/O fails."
  [ctx {:keys [loader-module-url preload-module-urls init-opts polyglot-context]}]
  (let [^Context pctx (or polyglot-context (context))]
    ;; The Context monitor first: a caller under with-graal-lock holds it
    ;; when it reaches here. kondo cannot see that the :module-ref atom is
    ;; shared per WasmContext.
    #_{:clj-kondo/ignore [:locking-suspicious-lock]}
    (locking pctx
      (locking (:module-ref ctx)
        (or @(:module-ref ctx)
            (do
              (when (nil? loader-module-url)
                (throw (ex-info "bootstrap-graal-module! requires :loader-module-url"
                                {:ctx (:library-key ctx)})))
              (doseq [url preload-module-urls :when url]
                (eval-module! pctx url))
              (let [m (.get (run-loader! (eval-module! pctx loader-module-url)
                                         init-opts (:library-key ctx)))]
                (reset! (:module-ref ctx) m)
                (log/info "Module ready for" (:library-key ctx))
                m)))))))

(defn module-eval-js
  "Evaluate JS `source` in the Context that owns `module`, under its
   monitor. A Value works only in the Context that built it, so evaluate a
   helper per Context instead of sharing one."
  [^Value module ^String source ^String js-name]
  (let [ctx (.getContext module)]
    (locking ctx
      (.eval ctx (.build (Source/newBuilder "js" source js-name))))))

(defn- byte-array-proxy
  "Read-only, no-copy ProxyArray over `b` that reads each byte as 0 to
   255."
  ^ProxyArray [^bytes b]
  (reify ProxyArray
    (get [_ index] (bit-and (aget b (int index)) 0xff))
    (set [_ _index _value]
      (throw (UnsupportedOperationException. "byte-array-proxy is read-only")))
    (getSize [_] (alength b))))

;; Evaluated per call in the target Context. The Engine source cache makes
;; that cheap.
(def ^:private uint8-array-copy-source
  (.build (Source/newBuilder "js"
                             "(src) => { const n = src.length; const a = new Uint8Array(n); for (let i = 0; i < n; i++) a[i] = src[i]; return a; }"
                             "clj-native-bytes.js")))

(defn js-bytes
  "Copy `b` into a JS Uint8Array in `ctx`, by default the shared Context.
   Bytes arrive unsigned. A Uint8Array works only in the Context that built
   it, so a pool worker passes its own."
  ([^bytes b] (js-bytes (context) b))
  ([^Context ctx ^bytes b]
   (locking ctx
     (.execute ^Value (.eval ctx uint8-array-copy-source)
               (into-array Object [(byte-array-proxy b)])))))

(defn js-bytes-map
  "Encode a name -> byte-array map as a JS object of Uint8Arrays in `ctx`,
   as js-bytes does."
  ([m] (js-bytes-map (context) m))
  ([^Context ctx m]
   (ProxyObject/fromMap
    (into {} (map (fn [[k v]] [(name k) (js-bytes ctx v)])) m))))

(defn heap-write-bytes!
  "Copy `b` into the HEAPU8 of `module` at wasm address `ptr`, one polyglot
   call per byte. Returns the byte count. It reads HEAPU8 itself, because
   memory growth detaches older views."
  ^long [^Value module ^long ptr ^bytes b]
  (with-module-lock module
    (let [heapu8 (.getMember module "HEAPU8")
          n      (alength b)]
      (dotimes [i n]
        (.setArrayElement heapu8 (+ ptr i) (bit-and (aget b i) 0xFF)))
      n)))

(defn heap-write-doubles!
  "Copy `xs` into the HEAPF64 of the current module at 8-byte-aligned wasm
   address `ptr`. Returns the count. It reads HEAPF64 itself, as
   heap-write-bytes! does."
  ^long [^long ptr ^doubles xs]
  (with-current-module [module]
    (let [heapf64 (.getMember module "HEAPF64")
          b (bit-shift-right ptr 3)
          n (alength xs)]
      (dotimes [i n]
        (.setArrayElement heapf64 (+ b i) (aget xs i)))
      n)))

(defn utf8->string
  "Read the NUL-terminated UTF-8 string at wasm address `ptr`, through the
   UTF8ToString export of `module`."
  ^String [^Value module ptr]
  (module-execute module "UTF8ToString" [ptr] :string))

(defn ccall
  "ccall `c-fn-name` on `module`. `rettype` and `argtypes` are emscripten
   ccall types, as keywords or strings. Returns the raw polyglot result."
  [^Value module c-fn-name rettype argtypes args]
  (module-execute module "ccall"
                  [c-fn-name
                   (name rettype)
                   ;; ccall indexes both lists as JS arrays. A host Object
                   ;; array reaches the C fn as no arguments, and the call
                   ;; still returns.
                   (ProxyArray/fromArray (object-array (map name argtypes)))
                   (ProxyArray/fromArray (object-array args))]))

(defn ccall-string
  "ccall a C fn that returns char *. Returns nil for NULL, where rettype
   \"string\" gives \"\". The read holds the call's lock, because another
   thread can free the string in between."
  [^Value module c-fn-name argtypes args]
  (with-module-lock module
    (let [addr (address-as-int (ccall module c-fn-name :number argtypes args))]
      (when-not (zero? addr)
        (utf8->string module addr)))))

(defn put-js-globals!
  "Put each entry of `m` on globalThis of `ctx`. Use it for a host callback
   that a C stub reads at call time."
  [^Context ctx m]
  (locking ctx
    (let [bindings (.getBindings ctx "js")]
      (doseq [[k v] m]
        (.putMember bindings (name k) v)))))

(defrecord TrackablePointer [address])

(defn address-as-int
  "The address of `p`, a TrackablePointer, a polyglot Value or a number, as
   an int."
  [p]
  (cond
    (instance? TrackablePointer p) (address-as-int (:address p))
    (instance? Value p)            (.asInt ^Value p)
    :else                          (int p)))

(defn address-as-trackable-pointer
  "`p` as a TrackablePointer. Throws when `p` is a polyglot Value that is
   not a number."
  [p]
  (if (instance? TrackablePointer p) p (->TrackablePointer (address-as-int p))))

(defn get-value
  "The value of emscripten `type`, such as \"i32\" or \"*\", at address `p`
   of the current module."
  [p type]
  (graal-execute "getValue" [(address-as-int p) type]))

(defn pointer->string
  "The NUL-terminated UTF-8 string at address `p` of the current module, or
   nil for NULL."
  [p]
  (let [a (address-as-int p)]
    (when-not (zero? a)
      (utf8->string (current-module) a))))

(defn string-array-pointer->strs
  "The strings of the NULL-terminated char** at address `p` of the current
   module, or [] for NULL."
  [p]
  (with-current-module [m]
    (let [get-value (.getMember m "getValue")
          utf8      (.getMember m "UTF8ToString")]
      (loop [slot (long (address-as-int p)), acc []]
        (let [a (if (zero? slot) 0 (.asInt (.execute get-value (object-array [slot "*"]))))]
          (if (zero? a)
            acc
            (recur (+ slot 4) (conj acc (.asString (.execute utf8 (object-array [a])))))))))))

(defn malloc
  "Allocate `b` bytes on the wasm heap. Returns a TrackablePointer that the
   caller frees with free-on-heap. Throws when `b` does not fit an i32 or
   the allocation fails."
  [b]
  (when-not (<= 0 b Integer/MAX_VALUE)
    (throw (ex-info (str "malloc size out of range: " b) {:size b})))
  (let [p (graal-execute "_malloc" [(int b)] :trackable-pointer)]
    (when (and (pos? b) (zero? (address-as-int p)))
      (throw (ex-info (str "malloc of " b " bytes failed") {:size b})))
    p))

(defn- write-pointer-slots!
  "Write the address of each of `ptrs` into 4-byte slots from `base`.
   Call it inside with-current-module. Returns `base`."
  ^long [^Value module ^long base ptrs]
  (let [set-value (.getMember module "setValue")]
    (dotimes [i (count ptrs)]
      (.execute set-value
                (into-array Object [(+ base (* i 4))
                                    (address-as-int (nth ptrs i))
                                    "*"])))
    base))

(defn string-list-to-native-array
  "Allocate a NULL-terminated char** of `s-list` on the wasm heap, in one
   block: the pointer slots, then the strings. An empty list gives the NULL
   slot alone, as on FFI. Returns a TrackablePointer that the caller frees
   with free-on-heap. Throws on a nil element."
  [s-list]
  (when (some nil? s-list)
    (throw (ex-info "string-list-to-native-array: s-list holds nil, which a char** cannot hold"
                    {:s-list s-list})))
  (let [sizes (mapv #(inc (alength (.getBytes ^String % "UTF-8"))) s-list)
        slots (* 4 (inc (count s-list)))
        base  (long (address-as-int (malloc (+ slots (reduce + sizes)))))
        addrs (vec (butlast (reductions + (+ base slots) sizes)))]
    (with-current-module [m]
      (let [to-utf8 (.getMember m "stringToUTF8")]
        (dotimes [i (count s-list)]
          (.execute to-utf8 (object-array [(nth s-list i) (nth addrs i) (nth sizes i)]))))
      (write-pointer-slots! m base (conj addrs 0)))
    (->TrackablePointer base)))

(defn free-on-heap
  "Free wasm heap pointer `ptr`. A nil `ptr` does nothing."
  [ptr]
  (when ptr
    (graal-execute "_free" [(address-as-int ptr)])))

(defn pointers->wasm-array
  "Pack the addresses of `ptrs` into a new table of 4-byte slots on the
   wasm heap, with no NULL terminator. Returns a TrackablePointer that the
   caller frees with free-on-heap."
  [ptrs]
  (let [arr (malloc (* 4 (count ptrs)))]
    (with-current-module [m]
      (write-pointer-slots! m (long (address-as-int arr)) ptrs))
    arr))

(defn read-heap-array
  "Read `n` elements at `ptr` from the `heap-type` view (:u8 :i8 :u16 :i16
   :u32 :i32 :f32 :f64) of the current module, into a Java primitive array
   of the same width. An unsigned type reads the signed view, which has the
   same bits."
  [ptr n heap-type]
  (with-current-module [module]
    (let [base (long (address-as-int ptr))]
      ;; Written out: a table loses the primitive-array types that
      ;; *warn-on-reflection* needs.
      (case heap-type
        (:u8 :i8)   (let [h ^Value (.getMember module "HEAP8") out (byte-array n)]
                      (dotimes [i n] (aset out i (unchecked-byte (.asInt (.getArrayElement h (+ base i))))))
                      out)
        (:u16 :i16) (let [h ^Value (.getMember module "HEAP16") b (bit-shift-right base 1) out (short-array n)]
                      (dotimes [i n] (aset out i (unchecked-short (.asInt (.getArrayElement h (+ b i))))))
                      out)
        (:u32 :i32) (let [h ^Value (.getMember module "HEAP32") b (bit-shift-right base 2) out (int-array n)]
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
  "Read the C struct at `struct-addr` in the current module into a map.
   `fields` is a vector of [key type offset], with type :string, :int,
   :double or :boolean. A NULL :string gives nil."
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
