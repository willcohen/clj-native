;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#?(:clj
   (ns net.willcohen.native.dispatch
     "Generic dispatch engine for one fn at a time, for native and wasm-backed
      C bindings. A consumer builds one library value with `library`, and
      calls `call!` for each fn. The library value carries the hooks. There is
      no registry.

      Three backends, on two axes. The host is the JVM or JavaScript. The
      compiled artifact is a native shared library or an emscripten .wasm.
      Three of the four cells exist, because a JS host cannot bind a native
      shared library.

        JVM host + native .so/.dylib -> :ffi   (Panama, through dt-ffi)
        JVM host + emscripten .wasm  -> :graal (GraalWasm, through ccall)
        JS host  + emscripten .wasm  -> CLJS   (worker pool, through ccall)

      The two wasm backends speak ccall because they share the ARTIFACT, and
      not the host. That is why `fn-record` below precomputes :ccall-rettype
      and :ccall-argtypes in shared code, and why the FFI backend ignores
      both. The FFI backend has real symbols, so it binds them directly and
      needs no type list at call time.

      JVM (GraalVM). Do a ccall on the module in scope: a bound
      *wasm-context* wins, and the registered module of the library is the
      fallback. The call holds the monitor of the Context that owns the
      module. An exception becomes a rettype-shaped nil. The rettype
      postprocess step wraps, dereferences or coerces the result for each
      type. ccall is an emscripten runtime export.
      net.willcohen.native.graal-wasm documents the full module contract.

      JVM (FFI). Apply the generated dt-ffi var through
      platform/call-native-fn. There is no ccall and no postprocess step.
      dt-ffi coerces by rettype in the generated wrapper.

      CLJS. Build a {:cmd \"ccall\"} envelope, with the extras from the
      extras-builder. Route it through pool/worker-call. Then apply
      :on-result, and then the result-wrapper."
     (:require [net.willcohen.native.graal-wasm :as nw]
               [net.willcohen.native.platform :as nplatform]
               [clojure.tools.logging :as log]
               [tech.v3.resource :as resource]))
   :cljs
   (ns net.willcohen.native.dispatch
     "Generic dispatch engine for one fn at a time, for wasm-backed C library
      bindings. Refer to the JVM namespace docstring for context. CLJS routes
      ccalls through the worker-router ref that the caller supplies. Library
      hooks build the extras, and they wrap the results."
     (:require ["./pool.mjs" :as pool]
               ["./handler_runtime.mjs" :as hrt])))

#?(:clj (set! *warn-on-reflection* true))

(defn argtype->ccall-type
  "Map a fndefs argument or return type keyword to the ccall type keyword.
   Use (name (argtype->ccall-type t)) when you must have a string.

   `library` validates the fndefs types before this map runs. Thus the
   default arm serves direct callers only."
  [t]
  (case t
    (:pointer :pointer? :string-array :string-array? :int32 :int64 :float64 :size-t :void) :number
    (:string :string?) :string
    :number))

(def supported-types
  "The fndefs type vocabulary that `library` accepts for :rettype and for
   argtypes. Refer to `library`."
  #{:pointer :pointer? :string-array :string-array? :int32 :int64 :float64
    :size-t :void :string :string?})

;; This message is written out, and not derived from supported-types.
;; (str :pointer) gives ":pointer" on the JVM, and "pointer" under squint,
;; where a keyword is already a string. A derived message would read
;; differently in the two lanes.
;; validate-fn-def!-names-every-supported-type pins this against the set.
(def ^:private supported-types-msg
  (str ":pointer :pointer? :string-array :string-array? :int32 :int64 "
       ":float64 :size-t :void :string :string?"))

(defn- validate-fn-def!
  "Throw when a fn-def carries a type outside the supported vocabulary.
   `library` runs this one time for each fn-key, at build time. Thus a bad
   type fails at assembly, and not at the first call."
  [fn-key fn-def]
  (let [rettype (:rettype fn-def)]
    (when-not (contains? supported-types rettype)
      (throw (ex-info (str "Unsupported :rettype " rettype " in fn-def " fn-key
                           ". Supported: " supported-types-msg)
                      {:fn-key fn-key :rettype rettype})))
    (doseq [[arg-name t] (:argtypes fn-def)]
      (when-not (contains? supported-types t)
        (throw (ex-info (str "Unsupported argtype " t " for arg " arg-name
                             " in fn-def " fn-key ". Supported: " supported-types-msg)
                        {:fn-key fn-key :arg arg-name :argtype t}))))))

(defn normalize-null-pointer
  "Give nil for a :pointer or :pointer? return with a raw address of 0. A
   zero address is true in JS and in Clojure, thus it defeats a (nil? x)
   test.

   The scope is call! return values only. A raw address from a heap read is
   still 0 for null. Thus null-ptr? keeps its zero branch.

   :pointer and :pointer? behave identically here. Thus the JVM-GraalVM
   backend must also treat them identically, in jvm-rettype-postprocess. That
   function also wraps a live address in a TrackablePointer."
  [rettype result]
  (if (and (or (= rettype :pointer) (= rettype :pointer?))
           (= 0 result))
    nil
    result))

;; The library value. A consumer builds one library value and keeps it. The
;; consumer gives that value to each call. No code finds a library by its name
;; at call time.

(defn- type-indexes
  "The set of indexes in `argtypes` whose type is `t`."
  [t argtypes]
  (set (keep-indexed (fn [i [_ at]] (when (= t at) i)) argtypes)))

(defn- fn-record
  "Calculate the data that a call must have from the fn-def alone. `library`
   does this one time for each fn-key."
  [fn-key fn-def]
  {:fn-key         fn-key
   :c-name         #?(:clj (name fn-key) :cljs (str fn-key))
   :fn-def         fn-def
   :rettype        (:rettype fn-def)
   :ccall-rettype  (argtype->ccall-type (:rettype fn-def))
   :ccall-argtypes (mapv (fn [[_ t]] (argtype->ccall-type t)) (:argtypes fn-def))
   :string?-indexes (type-indexes :string? (:argtypes fn-def))
   :int64-indexes   (type-indexes :int64 (:argtypes fn-def))})

(defn- update-at-indexes
  "`args` with `f` applied at each index in the set `ix`."
  [ix f args]
  (if (empty? ix)
    args
    (vec (map-indexed (fn [i a] (if (contains? ix i) (f a) a)) args))))

(defn library
  "Build a library value from the fndefs of a consumer. The consumer holds
   the returned map and passes it to call!. There is no global registry.

   Keys:
     :key         The library keyword. It routes pool affinity,
                  LibraryContext tracking, eviction, and WasmContext lookup.
     :fndefs      The fn-key -> fn-def map of the consumer.
     :impl-atom   An atom that holds :ffi, :graal, :node, or :browser. The
                  library value keeps a REFERENCE to this atom and reads it
                  at each call. Thus force-graal! has an effect with no
                  rebuild.
     :ffi-impl-ns The symbol of the namespace that holds the generated
                  dt-ffi vars.
     :hooks       {:extras-builder :result-wrapper :context-isolator
                  :result-check}. All hooks are optional. The
                  context-isolator returns a map. Dispatch reads only :args
                  from that map. Dispatch gives the full map to the
                  result-wrapper as :isolator-result. All other keys in the
                  map belong to the consumer.

   Type vocabulary. `library` validates each fn-def at build time. An
   unknown :rettype or argtype throws, with the supported set in the
   message.

   These types are valid in the two positions: :pointer :pointer?
   :string-array :string-array? :int32 :int64 :float64 :size-t :void
   :string :string?.

   :string? is a :string argument that can be nil (NULL), which dt-ffi's
   :string rejects. A NULL string result is nil on the JVM and \"\" on
   CLJS, where emscripten ccall gives \"\" for both.

   :int64 is a Long on the JVM. The wasm legs send it as a BigInt, as a
   WASM_BIGINT module (the emscripten default since 4.0.0) needs, and the
   CLJS leg returns the BigInt.

   clj-native has two other type vocabularies. read-heap-array has its heap
   views (:u8 :i8 :u16 :i16 :u32 :i32 :f32 :f64). read-struct has its field
   types (:string :int :double :boolean). The three stay separate on
   purpose, and each one names what its own layer reads.

   The shape of the returned value is API, and the dispatch suite pins it.
   :key, :impl-atom, :ffi-impl-ns and :hooks pass through unchanged.
   `library` replaces :fndefs with :fns. :fns maps each fn-key to a
   precomputed record of {:fn-key :c-name :fn-def :rettype :ccall-rettype
   :ccall-argtypes :string?-indexes :int64-indexes}.

   The returned value does not carry the raw fndefs map. Reach one fn-def
   through (get-in lib [:fns fn-key :fn-def]). A rename of any of these keys
   is a breaking change."
  [{:keys [key fndefs impl-atom ffi-impl-ns hooks]}]
  {:key         key
   :impl-atom   impl-atom
   :ffi-impl-ns ffi-impl-ns
   :hooks       (or hooks {})
   :fns         (reduce-kv (fn [m k v]
                             (validate-fn-def! k v)
                             (assoc m k (fn-record k v)))
                           {} fndefs)})

(defn ^:async check-result
  "Run the result-check hook of the library, if the library has one. Without
   a hook, return the result unchanged.

   The hook is (fn [library fn-key fn-def opts result]). It returns a new
   result, or it throws. An example is a throw on errno != 0 after a NULL
   return. On CLJS it can return a Promise, and check-result awaits it.

   The hook receives the library VALUE. Thus a hook that dispatches again
   does no lookup.

   A consumer calls check-result one time for each public fn. A hook that
   dispatches again must test fn-key, to prevent re-entry."
  [lib fn-key fn-def opts result]
  (if-let [f (:result-check (:hooks lib))]
    #?(:clj  (f lib fn-key fn-def opts result)
       :cljs (await (f lib fn-key fn-def opts result)))
    result))

#?(:clj
   (defn- convert-arg-jvm
     "Generic JVM argument conversion.
      - A pointerlike record (TrackablePointer and its variants) -> :address
      - An atom with {:ptr ...} (a context atom) -> the :address from :ptr
      - nil -> 0
      - Anything else -> unchanged"
     [arg]
     (cond
       (and (record? arg) (contains? arg :address)) (:address arg)
       (and (instance? clojure.lang.IDeref arg)
            (map? @arg)
            (contains? @arg :ptr))
       (let [ptr (:ptr @arg)]
         (if (and (record? ptr) (contains? ptr :address))
           (:address ptr)
           ptr))
       (nil? arg) 0
       :else arg)))

#?(:clj
   (defn jvm-rettype-postprocess
     "Coerce a GraalVM ccall result by rettype. A :pointer or :pointer?
      result becomes a TrackablePointer, or nil for a null address, which
      would defeat a (nil? x) test. :int32, :float64, :int64 and :size-t read
      a Polyglot Value as a number. :void gives nil. Any other result passes
      through: a string from nw/ccall-string, or a :string-array address for
      the caller to walk. nil stays nil."
     [rettype result]
     (case rettype
       (:pointer :pointer?) (when (some? result)
                              (let [tp (nw/address-as-trackable-pointer result)]
                                (when-not (zero? (nw/address-as-int tp)) tp)))
       :int32   (if (instance? org.graalvm.polyglot.Value result)
                  (nw/address-as-int result)
                  result)
       :float64 (if (instance? org.graalvm.polyglot.Value result)
                  (.asDouble ^org.graalvm.polyglot.Value result)
                  result)
       (:int64 :size-t) (if (instance? org.graalvm.polyglot.Value result)
                          (.asLong ^org.graalvm.polyglot.Value result)
                          result)
       :void    nil
       result)))

#?(:clj
   (defn- graal-module
     "The module of (nw/library-context library-key). Throws when it is not
      loaded."
     ^org.graalvm.polyglot.Value [library-key]
     (or (nw/get-module (nw/library-context library-key))
         (throw (ex-info "WasmContext has no loaded module"
                         {:library-key library-key})))))

#?(:clj
   (defn jvm-graal-call
     "ccall `c-fn-name` on the module of a bound *wasm-context*, else of
      `library-key`. A \"string\" rettype reads through nw/ccall-string.
      Logs an exception and returns nil."
     [library-key c-fn-name ccall-rettype ccall-argtypes converted-args]
     (let [module (graal-module library-key)]
       (try
         (if (= "string" (name ccall-rettype))
           (nw/ccall-string module c-fn-name ccall-argtypes converted-args)
           (nw/ccall module c-fn-name ccall-rettype ccall-argtypes converted-args))
         (catch Exception e
           (log/warn (str "Graal ccall exception for " c-fn-name ": " (.getMessage e)))
           nil)))))

#?(:clj
   (defn- ffi-leg
     "The FFI backend of call!. Each :string? argument becomes a C string
      that lives until the call returns."
     [lib rec fn-key args]
     (let [ix (:string?-indexes rec)]
       (if (empty? ix)
         (nplatform/call-native-fn (:ffi-impl-ns lib) fn-key args)
         (resource/stack-resource-context
          (nplatform/call-native-fn (:ffi-impl-ns lib) fn-key
                                    (update-at-indexes ix nplatform/nullable-c-string args)))))))

#?(:clj
   (defn- graal-int64-args
     "`args` with each :int64 argument as a BigInt of the module's Context,
      since a WASM_BIGINT module takes nothing else for an i64. The decimal
      string keeps all 64 bits."
     [library-key rec args]
     (let [ix (:int64-indexes rec)]
       (if (empty? ix)
         args
         (let [big-int (nw/module-eval-js (graal-module library-key) "BigInt" "bigint.js")]
           (update-at-indexes ix #(nw/value-execute big-int [(str (long %))]) args))))))

#?(:clj
   (defn- graal-leg
     "The JVM-GraalVM backend of call!. It reads the ccall types from the
      precomputed record. It does not calculate them again from the fn-def."
     [lib rec args]
     (let [rettype (:rettype rec)
           raw (jvm-graal-call (:key lib)
                               (:c-name rec)
                               (:ccall-rettype rec)
                               (:ccall-argtypes rec)
                               (graal-int64-args (:key lib) rec (mapv convert-arg-jvm args)))
           postprocessed (jvm-rettype-postprocess rettype raw)]
       (if-let [result-wrapper (:result-wrapper (:hooks lib))]
         (result-wrapper {:rettype rettype
                          :result postprocessed
                          :fn-def (:fn-def rec)
                          :args args
                          :platform :graal})
         postprocessed))))

#?(:cljs
   (defn- convert-arg-cljs
     "Generic CLJS argument conversion. The library extras-builder runs
      first, and it replaces library-specific shapes such as coord-arrays.
      - A map or JS object with :ptr -> that :ptr
      - nil -> 0
      - Anything else -> unchanged"
     [arg]
     ;; One arm covers the two shapes. The squint object? is a strict subset
     ;; of map?. map? returns true for everything that object? accepts, and
     ;; also for Map instances and the IMap types. (:ptr arg) reads the same
     ;; "ptr" key that (.-ptr arg) reads, because a single-word name has no
     ;; munging. A separate object? arm below this one is unreachable.
     (cond
       (and (map? arg) (:ptr arg)) (:ptr arg)
       (nil? arg) 0
       :else arg)))

#?(:cljs
   (defn- int64-args->bigint
     "`args` with each :int64 argument as a BigInt, for a WASM_BIGINT module."
     [rec args]
     (update-at-indexes (:int64-indexes rec) js/BigInt args)))

#?(:cljs
   (defn ^:async cljs-leg
     "The CLJS worker backend of call!. It reads the ccall types from the
      precomputed record. It reads the hooks from the library value."
     [lib rec args opts]
     (let [library-key (:key lib)
           hooks (:hooks lib)
           fn-key (:fn-key rec)
           c-fn-name (:c-name rec)
           fn-def (:fn-def rec)
           rettype (:rettype rec)
           ccall-rettype (:ccall-rettype rec)
           ccall-argtypes (:ccall-argtypes rec)
           result-wrapper (:result-wrapper hooks)
           extras-builder (:extras-builder hooks)
           pool-ref (:pool opts)
           force-idx (:force-worker-idx opts)
           worker-idx (if (some? force-idx)
                        force-idx
                        (pool/worker-idx-from-args library-key args))
             ;; Page-side substrate event. It captures the routing decision
             ;; of the call before dispatch. force-idx and worker-idx record
             ;; the affinity resolution. primary-handle is the identity
             ;; string that the consumer passes, and it does not go through
             ;; the ccall envelope. It lets a trace consumer correlate this
             ;; dispatch with the matching BUSY-INC and BUSY-DEC on the
             ;; worker side. The category is "dispatch", and the
             ;; setLogConfig categories list can filter it.
           _dispatch-resolve (hrt/dbg "DISPATCH-RESOLVE"
                                      #js {:lib (str library-key)
                                           :c-fn (str fn-key)
                                           :force-idx force-idx
                                           :worker-idx worker-idx
                                           :primary-handle (:primary-handle opts)})
             ;; The .ctx_id property name is the wire contract for handle
             ;; identity. The result-wrapper of a consumer sets it on a
             ;; wrapped handle. This scan reads it back, and so do the
             ;; refcount and eviction machinery of the pool. Worker affinity
             ;; has a registry (pool/register-worker-idx-predicate!),
             ;; because the extractors differ for each consumer. ctx_id
             ;; stays one fixed name on purpose, so do not add a second
             ;; registry for it.
           ctx-ids (vec (keep (fn [a]
                                (when (and (object? a) (some? (.-ctx_id a)))
                                  (.-ctx_id a)))
                              args))
           {builder-args :args
            extras       :extras
            on-result    :on-result} (if extras-builder
                                       (extras-builder fn-def args)
                                       {:args args :extras nil :on-result nil})
           on-result (or on-result identity)
             ;; Library-specific context isolator. It runs only when the
             ;; fn-def sets :isolate-context? true. Without that flag, the
             ;; argument passes through. The flag check prevents recursion
             ;; on the sub-dispatches of the isolator. Dispatch reads only
             ;; :args from the return value of the isolator. The rest is
             ;; consumer state, and the result-wrapper receives the full map
             ;; as :isolator-result. It emits ISOLATE-FIRE, so a trace can
             ;; confirm the isolator path.
           isolator-result (when (:isolate-context? fn-def)
                             (when-let [iso (:context-isolator hooks)]
                               (hrt/dbg "ISOLATE-FIRE" #js {:lib (str library-key)
                                                            :c-fn (str fn-key)
                                                            :worker worker-idx})
                               (await (iso {:fn-key fn-key
                                            :fn-def fn-def
                                            :args (or builder-args args)
                                            :worker-idx worker-idx
                                            :library-key library-key
                                            :library lib
                                            :pool pool-ref}))))
           isolator-args (get isolator-result :args (or builder-args args))
           converted (int64-args->bigint rec (mapv convert-arg-cljs isolator-args))
           ccall-cmd (cond-> {:cmd "ccall"
                              :fn c-fn-name
                              :returnType (str ccall-rettype)
                              :argTypes (mapv str ccall-argtypes)
                              :args converted}
                       (seq extras) (merge extras))]
         ;; An evicted ctx-id means a freed native handle. Fail cleanly
         ;; before any ref.
       (doseq [cid ctx-ids]
         (when (pool/evicted? library-key cid)
           (throw (ex-info (str "context " cid " was evicted (LRU); recreate it")
                           {:library-key library-key :ctx-id cid :evicted true}))))
         ;; Increment refcount and touched-at, so the LRU sweep skips an
         ;; in-flight ctx. unref fires in the finally on every exit path.
       (doseq [cid ctx-ids] (pool/ref-handle! library-key cid))
       (try
         (let [raw (await (pool/worker-call pool-ref
                                            library-key
                                            (:cmd ccall-cmd)
                                            (pool/cmd-args ccall-cmd)
                                            worker-idx))
               ;; Normalize before the result-wrapper, so the wrapper sees
               ;; one shape.
               postprocessed (normalize-null-pointer rettype (on-result raw))
               wrapped (if result-wrapper
                         (result-wrapper {:rettype rettype
                                          :result postprocessed
                                          :fn-def fn-def
                                          :args args
                                          :worker-idx worker-idx
                                          :platform :cljs
                                          :isolator-result isolator-result})
                         postprocessed)]
           wrapped)
           ;; Defense in depth. The wrap in handler_runtime.mjs normalizes
           ;; wasm traps before they cross the comlink RPC boundary. But a
           ;; future layer (pool or handler) can inject a non-cloneable
           ;; error downstream of that point. This catch normalizes again,
           ;; so the caller await always sees a propagatable Error.
         (catch :default e
           (throw (hrt/normalizeWasmError e)))
         (finally
           (doseq [cid ctx-ids] (pool/unref-handle! library-key cid)))))))

(defn ^:async call!
  "The single entry point for each fn. It reads the precomputed record. It
   selects the backend from the implementation atom of the library. Then it
   calls that backend.

   lib     A value from `library`.
   fn-key  A C function keyword, for example :mylib_create_context.
   args    The call arguments, as a vector.
   opts    Routing data for CLJS only. :pool is the worker-router ref, and
           CLJS must have it. :force-worker-idx replaces the affinity result.
           :primary-handle is a trace-correlation string, and it does not
           cross the worker boundary.

   call! does NOT initialize. Initialization belongs to the public surface of
   the consumer.

   Ownership. call! copies a :string return from native memory, and
   clj-native never frees the native side. clj-native never frees a :pointer
   return either. The consumer has the contract to free native memory,
   usually through the destroy functions of the library.

   This fn has one variadic arity, and not two arities. squint makes only the
   outer dispatcher of a multi-arity ^:async defn async. Thus an `await` in
   an inner arity body does not bundle."
  ;; opts carries CLJS-only routing. Refer to the docstring. The :clj reader
  ;; view never references it, thus the clj-kondo unused-binding fires under
  ;; :clj only.
  #_{:clj-kondo/ignore [:unused-binding]}
  [lib fn-key args & [opts]]
  (let [rec (get-in lib [:fns fn-key])]
    (when-not rec
      (throw (ex-info "Unknown fn-key for library"
                      {:fn-key fn-key :library (:key lib)})))
    #?(:clj
       (if (= :graal @(:impl-atom lib))
         (graal-leg lib rec args)
         (ffi-leg lib rec fn-key args))
       :cljs
       (await (cljs-leg lib rec args opts)))))

