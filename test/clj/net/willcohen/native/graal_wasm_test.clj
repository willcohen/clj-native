;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.graal-wasm-test
  "Tests of graal_wasm.clj against a real GraalVM Context and WebAssembly.Memory.
   test/fixtures/wasm-heap-loader.mjs supplies a hand-written module, so the
   suite needs no emcc."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [net.willcohen.native.dispatch :as dispatch]
            [net.willcohen.native.graal-wasm :as w])
  (:import [java.util.concurrent Callable CyclicBarrier ExecutorService]))

(def ^:private lib-key ::test-lib)

(def ^:private ctx (atom nil))

(defn- fixture-url [^String file]
  (-> (java.io.File. "test/fixtures" file) .toURI .toURL))

(defn- with-module-fixture [f]
  (let [c (w/create-wasm-context! lib-key)]
    (w/bootstrap-graal-module! c {:loader-module-url (fixture-url "wasm-heap-loader.mjs")})
    (reset! ctx c)
    (try
      (f)
      (finally
        ;; The sole-context fallback depends on the registry size.
        (swap! w/contexts dissoc lib-key)
        (reset! ctx nil)))))

(def ^:private pooled
  "{:wc :pctx}: a WasmContext, never registered, whose module lives in its
   own Context."
  (atom nil))

(defn- with-pooled-fixture [f]
  (let [pctx (w/new-polyglot-context!)
        wc   (w/->WasmContext ::pooled (atom nil))]
    (w/bootstrap-graal-module! wc {:loader-module-url (fixture-url "wasm-heap-loader.mjs")
                                   :polyglot-context pctx})
    (reset! pooled {:wc wc :pctx pctx})
    (try
      (f)
      (finally
        (reset! pooled nil)
        (.close pctx)))))

(use-fixtures :once with-module-fixture with-pooled-fixture)

(defmacro ^:private on-module [& body]
  `(w/with-wasm-context @ctx ~@body))

(defn- set-value!
  "Write `v` at `addr` through the module's own setValue."
  [addr v type]
  (let [m (w/get-module @ctx)]
    (.execute (.getMember m "setValue") (object-array [addr v type]))))

(defn- heap-string
  "The address of a UTF-8 copy of `s` on the heap of the current module."
  [s]
  (w/get-value (w/string-list-to-native-array [s]) "*"))

(deftest bootstrap-loads-the-module-and-is-idempotent
  (testing "a second bootstrap returns the same module without re-initializing"
    (let [again (w/bootstrap-graal-module! @ctx {:loader-module-url (fixture-url "wasm-heap-loader.mjs")})]
      (is (= (w/get-module @ctx) again))))
  (testing "bootstrap without a loader URL throws"
    (let [fresh (w/->WasmContext ::no-loader (atom nil))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (w/bootstrap-graal-module! fresh {}))))))

(deftest racing-first-calls-of-create-wasm-context!-share-the-registered-one
  (try
    (let [start (java.util.concurrent.CountDownLatch. 1)
          fs    (doall (repeatedly 32 #(future (.await start) (w/create-wasm-context! ::race))))]
      (.countDown start)
      (let [cs (mapv deref fs)]
        (is (every? #(identical? (get @w/contexts ::race) %) cs))))
    (finally (swap! w/contexts dissoc ::race))))

(deftest malloc-hands-out-usable-addresses
  (on-module
   (let [a (w/malloc 16)]
     (is (pos? (w/address-as-int a)) "a live allocation is not the null pointer")
     (testing "free-on-heap accepts a pointer and is nil-safe"
       (is (nil? (w/free-on-heap nil)))
       (w/free-on-heap a))
     (testing "a size that an i32 cannot hold throws, not wraps"
       (is (thrown? clojure.lang.ExceptionInfo (w/malloc 5000000000)))
       (is (thrown? clojure.lang.ExceptionInfo (w/malloc -1)))))))

(deftest string-round-trips-through-the-heap
  (on-module
   (doseq [s ["hello" "héllo wörld" ""]]
     (is (= s (w/pointer->string (heap-string s)))))
   (testing "NULL reads as nil, as on FFI"
     (is (nil? (w/pointer->string 0)))
     (is (= [] (w/string-array-pointer->strs 0))))))

(deftest string-list-to-native-array-builds-a-walkable-char**
  (on-module
   (testing "the array walks to its NULL terminator"
     (let [arr (w/string-list-to-native-array ["alpha" "beta" "gamma"])]
       (is (= ["alpha" "beta" "gamma"] (w/string-array-pointer->strs arr)))))
   (testing "one allocation, so one free-on-heap releases the array"
     (let [n (atom 0)
           malloc w/malloc]
       (with-redefs [w/malloc (fn [b] (swap! n inc) (malloc b))]
         (w/string-list-to-native-array ["alpha" "beta"]))
       (is (= 1 @n))))
   (testing "an empty list is a one-slot NULL array, as on FFI"
     (let [arr (w/string-list-to-native-array [])]
       (is (pos? (w/address-as-int arr)))
       (is (= [] (w/string-array-pointer->strs arr)))))
   (testing "a nil element throws rather than writing an unrepresentable char*"
     (is (thrown? clojure.lang.ExceptionInfo
                  (w/string-list-to-native-array ["ok" nil "also ok"]))))))

(deftest pointers->wasm-array-packs-4-byte-slots
  (on-module
   (let [p1   (w/malloc 8)
         p2   (w/malloc 8)
         p3   (w/malloc 8)
         arr  (w/pointers->wasm-array [p1 p2 p3])
         back (vec (w/read-heap-array arr 3 :i32))]
     (is (= [(w/address-as-int p1) (w/address-as-int p2) (w/address-as-int p3)] back)
         "each slot holds the wasm32 address of its pointer"))))

(deftest read-heap-array-covers-every-heap-type
  (on-module
   (let [p (w/malloc 64)
         a (w/address-as-int p)]
     (testing ":i8 and :u8 read the same bytes into byte[]"
       (set-value! a 65 "i8")
       (set-value! (+ a 1) 66 "i8")
       (is (= [65 66] (vec (w/read-heap-array p 2 :i8))))
       (is (= [65 66] (vec (w/read-heap-array p 2 :u8)))))
     (testing ":i16 and :u16 index by 2 bytes into short[]"
       (set-value! a 300 "i16")
       (set-value! (+ a 2) 301 "i16")
       (is (= [300 301] (vec (w/read-heap-array p 2 :i16))))
       (is (= [300 301] (vec (w/read-heap-array p 2 :u16)))))
     (testing ":i32 and :u32 index by 4 bytes into int[]"
       (set-value! a 70000 "i32")
       (set-value! (+ a 4) 70001 "i32")
       (is (= [70000 70001] (vec (w/read-heap-array p 2 :i32))))
       (is (= [70000 70001] (vec (w/read-heap-array p 2 :u32)))))
     (testing ":f32 into float[]"
       (set-value! a 1.5 "float")
       (set-value! (+ a 4) 2.5 "float")
       (is (= [1.5 2.5] (mapv double (w/read-heap-array p 2 :f32)))))
     (testing ":f64 indexes by 8 bytes into double[]"
       (set-value! a 1.25 "double")
       (set-value! (+ a 8) 2.5 "double")
       (is (= [1.25 2.5] (vec (w/read-heap-array p 2 :f64)))))
     (testing "an unsupported heap-type throws instead of returning garbage"
       (is (thrown? clojure.lang.ExceptionInfo (w/read-heap-array p 1 :i64)))))))

(deftest read-heap-array-widens-unsigned-views-by-bit-pattern
  ;; The JVM has no unsigned primitives, so :u16/:u32 keep the bit pattern.
  (on-module
   (let [p (w/malloc 16)
         a (w/address-as-int p)]
     (set-value! a -1 "i16")
     (is (= -1 (first (w/read-heap-array p 1 :u16)))
         ":u16 reads 65535 and narrows to short -1, same bits")
     (set-value! a -1 "i32")
     (is (= -1 (first (w/read-heap-array p 1 :u32)))
         ":u32 reads 4294967295 and narrows to int -1, same bits"))))

(deftest read-struct-reads-fields-by-offset
  (on-module
   (let [s   (w/malloc 32)
         a   (w/address-as-int s)
         txt (heap-string "field")]
     (set-value! a 42 "i32")
     (set-value! (+ a 4) 1 "i32")
     (set-value! (+ a 8) 6.25 "double")
     (set-value! (+ a 16) (w/address-as-int txt) "*")
     (set-value! (+ a 20) 0 "*")
     (let [m (w/read-struct a [[:n :int 0]
                               [:flag :boolean 4]
                               [:d :double 8]
                               [:name :string 16]
                               [:missing :string 20]])]
       (is (= 42 (:n m)))
       (is (true? (:flag m)))
       (is (= 6.25 (:d m)))
       (is (= "field" (:name m)))
       (testing "a :string field at a null address is nil, not an empty string"
         (is (nil? (:missing m))))))))

(deftest with-wasm-context-of-a-context-with-no-module-throws
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no loaded module"
                        (w/with-wasm-context (w/->WasmContext ::empty (atom nil)) (w/malloc 8)))))

(deftest current-module-falls-back-to-the-one-registered-context
  (with-redefs [w/contexts (atom {})]
    (let [only (w/create-wasm-context! ::sole)]
      (reset! (:module-ref only) (w/get-module @ctx))
      (is (pos? (w/address-as-int (w/malloc 8)))))))

;; test/fixtures/graal-load-loader.mjs returns a module that reports what its
;; `load` received.

(defn- boot-result
  "What a bootstrap of a new unregistered WasmContext with `opts` gives
   within 5 s: the module, a Throwable, or ::timeout."
  [opts]
  (let [c (w/->WasmContext ::boot (atom nil))]
    (deref (future (try (w/bootstrap-graal-module! c opts) (catch Throwable t t)))
           5000 ::timeout)))

(defn- boot-load [init-opts]
  (boot-result {:loader-module-url (fixture-url "graal-load-loader.mjs")
                :init-opts         init-opts}))

(defn- member [^org.graalvm.polyglot.Value v k]
  (let [m (.getMember v k)]
    (cond (.isBoolean m) (.asBoolean m)
          (.isNumber m) (.asLong m)
          (.isString m) (.asString m)
          :else m)))

(deftest bootstrap-accepts-a-load-contract-loader
  (let [db (byte-array (map unchecked-byte [7 8 200 255]))
        m (boot-load {"mode" "ok"
                      "dbBytes" (w/js-bytes db)
                      "grids" (w/js-bytes-map
                               {"a.gsb" (byte-array (map unchecked-byte [1]))
                                "b.gsb" (byte-array (map unchecked-byte [200 1]))})})]
    (testing "the promise the loader returned settled onto the caller's future"
      (is (= "load-contract" (member m "marker")))
      (is (= "ok" (member m "mode"))))
    (testing "js-bytes hands the loader a real Uint8Array, widened past 127"
      (is (true? (member m "dbIsUint8Array")))
      (is (= 4 (member m "dbLength")))
      (is (= 200 (member m "dbSecondLast")))
      (is (= 255 (member m "dbLast")))
      (is (= 4 (member m "dbBufferBytes"))))
    (testing "js-bytes-map keys each name to its own Uint8Array"
      (is (= "a.gsb,b.gsb" (member m "gridNames")))
      (is (= 200 (member m "gridFirstByte"))))))

(deftest bootstrap-accepts-a-load-that-returns-the-module-directly
  (let [m (boot-load {"mode" "sync"})]
    (is (= "load-contract" (member m "marker")))
    (is (= "sync" (member m "mode")))))

(deftest bootstrap-refuses-a-loader-that-gives-no-module
  (is (instance? Throwable
                 (boot-result {:loader-module-url (fixture-url "graal-empty-success.mjs")}))
      "onSuccess with no module")
  (is (instance? Throwable (boot-load {"mode" "undefined"}))
      "a load that resolves to undefined")
  (is (instance? Throwable (boot-load {"mode" "never"}))
      "a load whose promise never settles")
  (is (instance? java.util.concurrent.ExecutionException (boot-load {"mode" "throw"}))
      "a load that rejects")
  (is (re-find #"neither load nor initialize"
               (str (ex-message (boot-result {:loader-module-url (fixture-url "graal-no-loader.mjs")}))))
      "a module that exports neither load nor initialize"))

(deftest bootstrap-takes-the-context-lock-before-the-module-lock
  ;; Thread b is inside a bootstrap when thread a takes the Context monitor
  ;; and starts a bootstrap of the same WasmContext.
  (let [pctx      (w/new-polyglot-context!)
        wc        (w/->WasmContext ::lock-order (atom nil))
        boot      #(w/bootstrap-graal-module! wc {:loader-module-url (fixture-url "wasm-heap-loader.mjs")
                                                  :polyglot-context pctx})
        in-boot   (promise)
        holds-ctx (promise)
        eval-mod  @#'w/eval-module!]
    (with-redefs-fn {#'w/eval-module! (fn [c url]
                                        (deliver in-boot true)
                                        (deref holds-ctx 300 nil)
                                        (eval-mod c url))}
      (fn []
        (let [b (future (boot))
              a (future (deref in-boot 5000 nil)
                        (locking pctx (deliver holds-ctx true) (boot)))]
          (is (not-any? #{::timeout} (map #(deref % 5000 ::timeout) [a b])))
          (.close pctx))))))

(defn- module [] (w/get-module @ctx))

(defn- ccall-log
  "The stand-in ccall's record of call `idx`. :types and :vals are nil when
   the list did not arrive as a JS array."
  [idx]
  (let [entry (.getArrayElement (.getMember (module) "__ccalls") idx)
        lst   (fn [k]
                (let [v (.getMember entry k)]
                  (when-not (.isNull v)
                    (mapv #(let [e (.getArrayElement v %)]
                             (if (.isString e) (.asString e) (.asInt e)))
                          (range (.getArraySize v))))))]
    {:name    (.asString (.getMember entry "name"))
     :rettype (.asString (.getMember entry "rettype"))
     :types   (lst "types")
     :vals    (lst "vals")}))

(defn- ccall-count [] (.getArraySize (.getMember (module) "__ccalls")))

(deftest heap-write-bytes!-copies-at-the-given-address
  (let [m    (module)
        ptr  (long (w/address-as-int (on-module (w/malloc 8))))
        src  (byte-array (map unchecked-byte [0 1 127 -128 -1 200 42 255]))
        n    (w/heap-write-bytes! m ptr src)]
    (testing "every byte lands, widened to its unsigned value"
      (is (= 8 n))
      (is (= [0 1 127 128 255 200 42 255]
             (mapv #(bit-and % 0xff)
                   (on-module (w/read-heap-array ptr 8 :u8))))))))

(deftest value-execute-runs-a-free-standing-fn-with-coercion
  (let [f (on-module (w/module-eval-js (module) "(a, b) => a - b" "vx.js"))]
    (is (= 4 (w/value-execute f [7 3] :int))))
  (testing "value->long keeps an i64-range number that an int read cannot"
    (is (= 5000000000 (w/value->long (w/module-eval-js (module) "5000000000" "v64.js"))))))

(deftest address-as-trackable-pointer-throws-on-a-non-number
  (is (thrown? Exception (w/address-as-trackable-pointer (w/module-eval-js (module) "undefined" "u.js")))))

(deftest heap-write-doubles!-resolves-the-current-module
  (let [ptr (long (w/address-as-int (on-module (w/malloc 16))))]
    (on-module (w/heap-write-doubles! ptr (double-array [3.5 -4.5])))
    (is (= [3.5 -4.5] (vec (on-module (w/read-heap-array ptr 2 :f64)))))))

(deftest module-eval-js-targets-the-owning-context
  (let [m (module)]
    (w/module-eval-js m "globalThis.__mejs = 41 + 1" "mejs-probe.js")
    (is (= 42 (.asInt (w/module-eval-js m "globalThis.__mejs" "mejs-read.js"))))
    (testing "a pooled Context does not see the default-Context global"
      (is (.isNull (w/module-eval-js (w/get-module (:wc @pooled))
                                     "globalThis.__mejs ?? null"
                                     "mejs-read2.js"))))))

(deftest ccall-hands-its-lists-across-as-js-arrays
  ;; A host array reaches JS with no length or index access, so the C function
  ;; gets nothing and the call still returns. The fixture records it as null.
  (let [idx (ccall-count)
        r   (w/ccall (module) "sum" :number [:number :number] [20 22])]
    (testing "the arguments reach the C function"
      (is (= 42 (.asInt r))))
    (testing "both lists arrive as JS arrays, so neither reads as absent"
      (is (= {:name "sum" :rettype "number"
              :types ["number" "number"] :vals [20 22]}
             (ccall-log idx)))))
  (testing "a type named as a string is accepted alongside a keyword"
    (let [idx (ccall-count)]
      (w/ccall (module) "sum" "number" ["number" :number] [1 2])
      (is (= ["number" "number"] (:types (ccall-log idx))))))
  (testing "an empty argument list is still a JS array, not an absent one"
    (let [idx (ccall-count)]
      (is (= 0 (.asInt (w/ccall (module) "noop" :number [] []))))
      (is (= {:name "noop" :rettype "number" :types [] :vals []}
             (ccall-log idx))))))

(deftest jvm-graal-call-respects-the-wasm-context-binding
  ;; A pointer is valid only in the Context that made it, so a pool worker's
  ;; ccall must land on its bound module.
  (let [{:keys [wc]}   @pooled
        pooled-count   #(.getArraySize (.getMember (w/get-module wc) "__ccalls"))
        default-before (ccall-count)
        pooled-before  (pooled-count)]
    (w/with-wasm-context wc
      (dispatch/jvm-graal-call lib-key "stub_fn" "number" ["number"] [7]))
    (is (= default-before (ccall-count))
        "the registered default module saw no ccall")
    (is (= (inc pooled-before) (pooled-count))
        "the ccall landed on the bound pooled module")))

(deftest the-default-and-a-pooled-context-share-one-engine
  ;; The builder's Engine is a different object from the current-API
  ;; wrapper, and .equals does not bridge them. Compare wrappers.
  (is (identical? (.getEngine (w/context))
                  (.getEngine (.getContext (w/get-module (:wc @pooled)))))))

(deftest put-js-globals!-publishes-callbacks-a-c-stub-can-reach
  (let [called (atom nil)
        cb (reify org.graalvm.polyglot.proxy.ProxyExecutable
             (execute [_ args]
               (let [v (.asInt (aget args 0))]
                 (reset! called v)
                 (* 2 v))))]
    (w/put-js-globals! (w/context) {"__clj_native_test_cb" cb})
    (testing "the C stub route reaches the published callback with its argument"
      (is (= 42 (.asInt (w/ccall (module) "call_global" :number
                                 ["string" "number"]
                                 ["__clj_native_test_cb" 21]))))
      (is (= 21 @called)))))

;; emscripten reads /dev/urandom through crypto.getRandomValues, which
;; GraalJS lacks.
(def ^:private random-probe
  "(() => {
     const a = new Uint8Array(64);
     const b = new Uint32Array(16);
     const r = crypto.getRandomValues(a);
     crypto.getRandomValues(b);
     return [typeof crypto.getRandomValues, r === a,
             a.some((x) => x !== 0), b.some((x) => x > 255)].join(',');
   })()")

(def ^:private unaligned-probe
  "(() => {
     const buf = new ArrayBuffer(32);
     const v = new Uint8Array(buf, 3, 13);
     crypto.getRandomValues(v);
     const all = new Uint8Array(buf);
     return [all.slice(0, 3).every((x) => x === 0),
             all.slice(16).every((x) => x === 0),
             v.some((x) => x !== 0)].join(',');
   })()")

(deftest each-context-has-crypto-get-random-values
  (testing "a view at an offset with a tail shorter than 8 bytes"
    (is (= "true,true,true" (str (.eval (w/context) "js" unaligned-probe)))))
  (testing "the default Context"
    (is (= "function,true,true,true" (str (.eval (w/context) "js" random-probe)))))
  (testing "a pooled Context"
    (is (= "function,true,true,true"
           (str (.eval ^org.graalvm.polyglot.Context (:pctx @pooled) "js" random-probe))))))

(deftest call!-on-graal-tells-a-null-string-from-an-empty-string
  ;; ccall's "string" rettype gives "" for NULL. The FFI backend gives nil.
  (let [lib (dispatch/library {:key lib-key
                               :fndefs {:str_null  {:rettype :string :argtypes []}
                                        :str_empty {:rettype :string :argtypes []}
                                        :str_abc   {:rettype :string :argtypes []}}
                               :impl-atom (atom :graal)})]
    (on-module
     (is (nil? (dispatch/call! lib :str_null [])) "a NULL char* reads as nil")
     (is (= "" (dispatch/call! lib :str_empty [])) "an empty string stays \"\"")
     (is (= "abc" (dispatch/call! lib :str_abc []))))))

(deftest call!-on-graal-sends-a-nil-string?-argument-as-null
  ;; emscripten ccall sends 0 for a "string" argument as NULL.
  (let [lib (dispatch/library {:key lib-key
                               :fndefs {:noop {:rettype :int32
                                               :argtypes [[:key :string?]]}}
                               :impl-atom (atom :graal)})
        idx (ccall-count)]
    (on-module (dispatch/call! lib :noop [nil]))
    (is (= {:name "noop" :rettype "number" :types ["string"] :vals [0]}
           (ccall-log idx)))))

(deftest call!-on-graal-reads-an-int64-return-as-a-long
  ;; A WASM_BIGINT module gives an i64 result as a BigInt.
  (let [lib (dispatch/library {:key lib-key
                               :fndefs {:i64_ret {:rettype :int64 :argtypes []}}
                               :impl-atom (atom :graal)})
        r   (on-module (dispatch/call! lib :i64_ret []))]
    (is (instance? Long r))
    (is (= 3000000000 r) "all 64 bits, above 2^31")))

(deftest call!-on-graal-passes-and-returns-a-float32
  ;; The f32 export narrows the number, so 0.1 comes back as the nearest f32.
  (let [lib (dispatch/library {:key lib-key
                               :fndefs {:f32_id {:rettype :float32
                                                 :argtypes [[:v :float32]]}}
                               :impl-atom (atom :graal)})
        r   (on-module (dispatch/call! lib :f32_id [0.1]))]
    (is (instance? Double r) "a Double, not a Polyglot Value")
    (is (= (double (float 0.1)) r))))

(deftest call!-on-graal-sends-an-int64-argument-as-a-bigint
  ;; A WASM_BIGINT module rejects a JS number for an i64 parameter.
  (let [lib (dispatch/library {:key lib-key
                               :fndefs {:i64_echo {:rettype :int64
                                                   :argtypes [[:v :int64]]}}
                               :impl-atom (atom :graal)})]
    (is (= 3000000001 (on-module (dispatch/call! lib :i64_echo [3000000000])))
        "all 64 bits go in and come back")
    (is (= 9007199254740994 (on-module (dispatch/call! lib :i64_echo [9007199254740993])))
        "above 2^53, where a double loses the low bit")
    (is (= 1 (on-module (dispatch/call! lib :i64_echo [nil])))
        "nil goes in as 0")))

(deftest call!-on-graal-throws-when-the-ccall-throws
  ;; jvm-graal-call logged the exception and gave nil, which hid a clj-gdal
  ;; OGR_F_SetFieldInteger64 write that never happened.
  (let [lib (dispatch/library {:key lib-key :impl-atom (atom :graal)
                               :fndefs {:i64_echo {:rettype :int64 :argtypes [[:v :int32]]}}})]
    (is (thrown-with-msg? Exception #"i64_echo" (on-module (dispatch/call! lib :i64_echo [7]))))))

(deftest call!-on-graal-names-a-library-with-no-module
  (let [k   ::no-module
        lib (dispatch/library {:key k
                               :fndefs {:noop {:rettype :int32 :argtypes []}}
                               :impl-atom (atom :graal)})]
    (w/create-wasm-context! k)
    (try
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no loaded module"
                            (dispatch/call! lib :noop [])))
      (finally (swap! w/contexts dissoc k)))))

(defn- jar-with-modules
  "A temp jar that holds each {path text} entry of `entries`."
  ^java.io.File [entries]
  (let [f (java.io.File/createTempFile "modules" ".jar")]
    (.deleteOnExit f)
    (with-open [out (java.util.jar.JarOutputStream. (java.io.FileOutputStream. f))]
      (doseq [[^String path ^String text] entries]
        (.putNextEntry out (java.util.jar.JarEntry. path))
        (.write out (.getBytes text "UTF-8"))
        (.closeEntry out)))
    f))

(deftest bootstrap-loads-modules-from-a-jar
  ;; A consumer jar holds its loader and its emscripten module as jar:
  ;; resources, and the loader imports the module by a relative path.
  (let [jar (jar-with-modules
             {"js/sibling.mjs" "export function marker() { return 'from-jar'; }"
              "js/loader.mjs"  (str "import { marker } from './sibling.mjs';\n"
                                    "export function load() { return { marker: marker() }; }")})
        cl  (java.net.URLClassLoader. (into-array java.net.URL [(.toURL (.toURI jar))]) nil)
        url #(.getResource cl %)
        m   (boot-result {:loader-module-url   (url "js/loader.mjs")
                          :preload-module-urls [(url "js/sibling.mjs")]})]
    (is (= "from-jar" (member m "marker")))))

(deftest module-file-of-a-jar-url-is-safe-across-threads
  ;; Pool workers bootstrap their Contexts at the same time, and each one
  ;; asks for the same jar: modules.
  (let [text (apply str (repeat 20000 "x"))
        jar  (jar-with-modules {"js/big.mjs" text})
        url  (.getResource (java.net.URLClassLoader. (into-array java.net.URL [(.toURL (.toURI jar))]) nil)
                           "js/big.mjs")
        n    8
        gate (CyclicBarrier. n)
        pool (java.util.concurrent.Executors/newFixedThreadPool n)
        read-copy (fn [] (.await gate) (slurp (#'w/module-file url)))]
    (try
      (let [results (->> (repeat (* 4 n) read-copy)
                         (map #(.submit ^ExecutorService pool ^Callable (fn [] (try (%) (catch Throwable t t)))))
                         doall
                         (mapv #(.get ^java.util.concurrent.Future %)))]
        (is (= #{(count text)} (set (map #(if (string? %) (count %) (class %)) results)))
            "each thread reads the whole copy"))
      (finally (.shutdownNow pool)))))

(defn- url-with-streams
  "A URL of a non-file scheme whose each open calls `open-stream`."
  ^java.net.URL [path open-stream]
  (java.net.URL. nil (str "test-stream:" path)
                 (proxy [java.net.URLStreamHandler] []
                   (openConnection [u]
                     (proxy [java.net.URLConnection] [u]
                       (connect [])
                       (getInputStream [] (open-stream)))))))

(defn- throwing-stream
  ^java.io.InputStream []
  (proxy [java.io.InputStream] []
    (read
      ([] (throw (java.io.IOException. "cut")))
      ([_] (throw (java.io.IOException. "cut")))
      ([_ _ _] (throw (java.io.IOException. "cut"))))))

(deftest module-file-keeps-no-partial-copy
  ;; A copy that fails partway must not stay, or each later bootstrap in the
  ;; JVM imports the truncated module.
  (let [text  (apply str (repeat 1000 "x"))
        bs    (.getBytes text "UTF-8")
        opens (atom 0)
        url   (url-with-streams
               (str "dir-" (random-uuid) "/m.mjs")
               #(if (= 1 (swap! opens inc))
                  (java.io.SequenceInputStream. (java.io.ByteArrayInputStream. bs 0 500)
                                                (throwing-stream))
                  (java.io.ByteArrayInputStream. bs)))]
    (is (thrown? java.io.IOException (#'w/module-file url)))
    (is (= text (slurp (#'w/module-file url))))))

(deftest with-library-context-picks-the-module-of-a-library
  ;; With two libraries registered, an unbound heap call cannot know which
  ;; module to use. clj-gdal and clj-proj in one JVM hit this.
  (let [_ (w/create-wasm-context! ::other-library)]
    (try
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"with-library-context"
                            (w/malloc 8))
          "the error names the fix")
      (is (pos? (w/address-as-int (w/with-library-context lib-key (w/malloc 8)))))
      (testing "a bound context stays, as for a pool worker"
        (let [{:keys [wc]} @pooled]
          (is (identical? (w/get-module wc)
                          (w/with-wasm-context wc
                            (w/with-library-context lib-key (#'w/current-module)))))))
      (finally (swap! w/contexts dissoc ::other-library)))))
