;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#?(:clj
   (ns net.willcohen.native.dispatch
     "Calls C fns one at a time, on native and wasm builds. A consumer builds
      a library value with `library` and passes it to `call!`. There is no
      registry.

        JVM + native .so/.dylib -> :ffi   (Panama, through dt-ffi)
        JVM + emscripten .wasm  -> :graal (GraalWasm, through ccall)
        JS  + emscripten .wasm  -> CLJS   (worker pool, through ccall)

      The two wasm backends share the artifact, so both use the ccall types
      that `fn-record` precomputes. The FFI backend binds real symbols and
      ignores them."
     (:require [clojure.string :as string]
               [net.willcohen.native.graal-wasm :as nw]
               [net.willcohen.native.platform :as nplatform]
               [tech.v3.resource :as resource]))
   :cljs
   (ns net.willcohen.native.dispatch
     "Calls C fns one at a time on wasm builds. A consumer builds a library
      value with `library` and passes it to `call!`, which routes a ccall
      through the worker-router pool in the :pool opt."
     (:require ["./pool.mjs" :as pool]
               ["./handler_runtime.mjs" :as hrt]
               [clojure.string :as string])))

#?(:clj (set! *warn-on-reflection* true))

(defn argtype->ccall-type
  [t]
  (case t
    (:string :string?) :string
    :number))

(def supported-types
  "The fndefs types that `library` accepts for :rettype and argtypes."
  #{:pointer :pointer? :int32 :int64 :float64 :size-t :void :string :string?})

;; (str :pointer) is ":pointer" on the JVM and "pointer" under squint.
(def ^:private supported-types-msg
  (string/join " " (map #(str ":" (name %)) (sort supported-types))))

(defn- validate-fn-def!
  "Throw ex-info when `fn-def` has a type outside supported-types."
  [fn-key fn-def]
  (doseq [[arg-name t] (cons [":rettype" (:rettype fn-def)] (:argtypes fn-def))]
    (when-not (contains? supported-types t)
      (throw (ex-info (str "Unsupported type " t " for " arg-name
                           " in fn-def " fn-key ". Supported: " supported-types-msg)
                      {:fn-key fn-key :arg arg-name :type t})))))

(defn normalize-null-pointer
  "nil for a :pointer or :pointer? result of 0, else `result`. A 0 address
   is logical true, so it defeats a nil? test. This applies to call! results
   only: a heap read still gives 0 for null. jvm-rettype-postprocess must
   treat the two types the same way."
  [rettype result]
  (if (and (or (= rettype :pointer) (= rettype :pointer?))
           (= 0 result))
    nil
    result))

(defn- type-indexes
  "The set of indexes in `argtypes` whose type is `t`."
  [t argtypes]
  (set (keep-indexed (fn [i [_ at]] (when (= t at) i)) argtypes)))

(defn- fn-record
  "The call data that `library` precomputes for one fn-def."
  [fn-key fn-def]
  {:fn-key          fn-key
   :c-name          (name fn-key)
   :fn-def          fn-def
   :rettype         (:rettype fn-def)
   :ccall-rettype   (argtype->ccall-type (:rettype fn-def))
   :ccall-argtypes  (mapv (fn [[_ t]] (argtype->ccall-type t)) (:argtypes fn-def))
   :string?-indexes (type-indexes :string? (:argtypes fn-def))
   :int64-indexes   (type-indexes :int64 (:argtypes fn-def))})

(defn- update-at-indexes
  "`args` with `f` applied at each index in the set `ix`."
  [ix f args]
  (if (empty? ix)
    args
    (vec (map-indexed (fn [i a] (if (contains? ix i) (f a) a)) args))))

(defn library
  "Build a library value for call!. Treat it as opaque.

     :key         Library keyword. It keys pool affinity, context tracking,
                  eviction and the WasmContext lookup.
     :fndefs      Map of fn-key to fn-def, each type one of supported-types.
                  An unknown type throws here, at build time.
     :impl-atom   (JVM) Atom of :ffi or :graal, read at each call, so
                  force-graal! needs no rebuild.
     :ffi-impl-ns Symbol of the ns that holds the generated dt-ffi vars.
     :hooks       Optional {:extras-builder :result-wrapper
                  :context-isolator :result-check}. Dispatch reads only
                  :args from the map that the context-isolator returns, and
                  passes the full map to the result-wrapper as
                  :isolator-result.

   :string? is a :string that can be nil (NULL), which dt-ffi's :string
   rejects. A NULL result is nil on the JVM and \"\" on CLJS, where ccall
   gives \"\" for both.
   :int64 is a Long on the JVM. The wasm backends send a BigInt, which a
   WASM_BIGINT module (the emscripten default since 4.0.0) needs. CLJS
   returns the BigInt."
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
  "Pass `result` through the :result-check hook of `lib`, if it has one.

   The hook is (fn [library fn-key fn-def opts result]). It returns a new
   result or throws, for example on errno after a NULL return. On CLJS it
   can return a Promise. A hook that dispatches again must test fn-key, to
   prevent re-entry."
  [lib fn-key fn-def opts result]
  (if-let [f (:result-check (:hooks lib))]
    #?(:clj  (f lib fn-key fn-def opts result)
       :cljs (await (f lib fn-key fn-def opts result)))
    result))

#?(:clj
   (defn- convert-arg-jvm
     "A pointer record gives its :address. nil gives 0. Anything else passes
      through."
     [arg]
     (cond
       (and (record? arg) (contains? arg :address)) (:address arg)
       (nil? arg) 0
       :else arg)))

#?(:clj
   (defn jvm-rettype-postprocess
     "Coerce a GraalVM ccall result by rettype. :pointer and :pointer? give
      a TrackablePointer, or nil for address 0. The numeric types read a
      Polyglot Value as a number. :void gives nil. Other results pass
      through."
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
      `library-key`. A :string rettype reads through nw/ccall-string. Throws
      an ex-info that names `c-fn-name` when the ccall throws."
     [library-key c-fn-name ccall-rettype ccall-argtypes converted-args]
     (let [module (graal-module library-key)]
       (try
         (if (= :string ccall-rettype)
           (nw/ccall-string module c-fn-name ccall-argtypes converted-args)
           (nw/ccall module c-fn-name ccall-rettype ccall-argtypes converted-args))
         (catch Exception e
           (throw (ex-info (str "ccall " c-fn-name " failed: " (ex-message e))
                           {:library-key library-key :c-fn-name c-fn-name} e)))))))

#?(:clj
   (defn- call-ffi
     "The FFI backend of call!. Each :string? argument becomes a C string
      that lives until the call returns."
     [lib rec args]
     (let [ix (:string?-indexes rec)
           call #(nplatform/call-native-fn (:ffi-impl-ns lib) (:fn-key rec) %)]
       (if (empty? ix)
         (call args)
         (resource/stack-resource-context
          (call (update-at-indexes ix nplatform/nullable-c-string args)))))))

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
   (defn- call-graal
     "The GraalVM backend of call!."
     [lib rec args]
     (jvm-rettype-postprocess
      (:rettype rec)
      (jvm-graal-call (:key lib)
                      (:c-name rec)
                      (:ccall-rettype rec)
                      (:ccall-argtypes rec)
                      (graal-int64-args (:key lib) rec (mapv convert-arg-jvm args))))))

#?(:cljs
   (defn- convert-arg-cljs
     "A map or JS object with :ptr gives that :ptr. nil gives 0. Anything
      else passes through. The extras-builder runs first."
     [arg]
     ;; One arm covers JS objects too: squint's map? accepts every object?,
     ;; and (:ptr arg) reads the same key as (.-ptr arg).
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
   (defn- ctx-ids
     "The .ctx_id of each argument that has one. A consumer result-wrapper
      sets it; the pool refcount and eviction read it."
     [args]
     (vec (keep (fn [a] (when (and (object? a) (some? (.-ctx_id a))) (.-ctx_id a)))
                args))))

#?(:cljs
   (defn- check-not-evicted!
     "Throw when an LRU eviction freed the native handle of one of `ids`."
     [library-key ids]
     (doseq [cid ids]
       (when (pool/evicted? library-key cid)
         (throw (ex-info (str "context " cid " was evicted (LRU); recreate it")
                         {:library-key library-key :ctx-id cid :evicted true}))))))

#?(:cljs
   (defn- ccall-args
     "The worker-call args of a ccall: the C fn name, the return type, the
      arg types, `args`, and an object of each non-nil entry of `extras`."
     [rec args extras]
     (let [extra (js-obj)]
       (doseq [[k v] extras]
         (when (some? v) (aset extra (str k) v)))
       #js [(:c-name rec)
            (str (:ccall-rettype rec))
            (mapv str (:ccall-argtypes rec))
            args
            extra])))

#?(:cljs
   (defn- ^:async isolate
     "The map that the :context-isolator hook returns for a fn-def with
      :isolate-context?, else nil. The flag also stops recursion on the
      sub-dispatches of the isolator."
     [lib rec args worker-idx pool-ref]
     (when (:isolate-context? (:fn-def rec))
       (when-let [iso (:context-isolator (:hooks lib))]
         (hrt/dbg "ISOLATE-FIRE" #js {:lib (str (:key lib)) :c-fn (:c-name rec) :worker worker-idx})
         (await (iso {:fn-key (:fn-key rec)
                      :fn-def (:fn-def rec)
                      :args args
                      :worker-idx worker-idx
                      :library-key (:key lib)
                      :library lib
                      :pool pool-ref}))))))

#?(:cljs
   (defn- ^:async call-cljs
     "The CLJS worker backend of call!."
     [lib rec args opts]
     (let [library-key (:key lib)
           hooks       (:hooks lib)
           pool-ref    (:pool opts)
           force-idx   (:force-worker-idx opts)
           worker-idx  (if (some? force-idx) force-idx (pool/worker-idx-from-args args))
           ids         (ctx-ids args)]
       ;; :primary-handle matches this call to its worker BUSY-INC and
       ;; BUSY-DEC in a trace. The worker never gets it.
       (hrt/dbg "DISPATCH-RESOLVE" #js {:lib (str library-key)
                                         :c-fn (:c-name rec)
                                         :force-idx force-idx
                                         :worker-idx worker-idx
                                         :primary-handle (:primary-handle opts)})
       (check-not-evicted! library-key ids)
       ;; Ref each ctx so the LRU sweep skips it while in flight.
       (doseq [cid ids] (pool/ref-handle! library-key cid))
       (try
         (let [{built :args extras :extras on-result :on-result}
               (if-let [b (:extras-builder hooks)]
                 (b (:fn-def rec) args)
                 {:args args})
               isolated (await (isolate lib rec (or built args) worker-idx pool-ref))
               call-args (int64-args->bigint rec (mapv convert-arg-cljs
                                                       (get isolated :args (or built args))))
               raw (await (pool/worker-call pool-ref library-key "ccall"
                                            (ccall-args rec call-args extras) worker-idx))
               ;; Normalize before the result-wrapper, so the wrapper sees one
               ;; shape.
               result (normalize-null-pointer (:rettype rec) ((or on-result identity) raw))]
           (if-let [wrap (:result-wrapper hooks)]
             (wrap {:rettype (:rettype rec)
                    :result result
                    :fn-def (:fn-def rec)
                    :args args
                    :worker-idx worker-idx
                    :platform :cljs
                    :isolator-result isolated})
             result))
         ;; handler_runtime.mjs already normalizes wasm traps, but a later layer
         ;; can inject a non-cloneable error, so normalize again.
         (catch :default e
           (throw (hrt/normalizeWasmError e)))
         (finally
           (doseq [cid ids] (pool/unref-handle! library-key cid)))))))

(defn ^:async call!
  "Call C fn `fn-key` of `lib` with the vector `args`: on the JVM through
   the backend that :impl-atom selects, on CLJS through the worker pool.
   Throws on an unknown fn-key. Does not initialize the library.

   Hooks: :extras-builder, :context-isolator and :result-wrapper run on CLJS
   only. call! never runs :result-check; call check-result. A ccall
   exception throws on :graal and rejects on CLJS.

   opts (CLJS only): :pool, the worker-router ref (required);
   :force-worker-idx, which overrides affinity; :primary-handle, a trace
   string that stays on the page.

   A :string result is a copy. clj-native never frees a native :string or
   :pointer result. The consumer frees it, usually through the destroy fns
   of the library."
  ;; :clj never reads opts.
  #_{:clj-kondo/ignore [:unused-binding]}
  [lib fn-key args & [opts]]
  (let [rec (get-in lib [:fns fn-key])]
    (when-not rec
      (throw (ex-info "Unknown fn-key for library"
                      {:fn-key fn-key :library (:key lib)})))
    #?(:clj
       (if (= :graal @(:impl-atom lib))
         (call-graal lib rec args)
         (call-ffi lib rec args))
       :cljs
       (await (call-cljs lib rec args opts)))))
