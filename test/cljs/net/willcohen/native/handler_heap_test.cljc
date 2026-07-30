;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.handler-heap-test
  "Coverage for handler-heap's methods object and its plain ccall method.

   The stand-in module is backed by one real ArrayBuffer with all eight
   emscripten heap views over it, so a write through one view is visible
   through another and the get/set pairs are tested against real memory rather
   than a recorded call list. Every method is async, as handler-runtime
   requires, so each assertion awaits.

   The late-binding thunk is under test too: the object is built while getModule
   still returns null, which is the order a consumer builds it in."
  (:require [cljs.test :refer [deftest is testing]]
            ["ffi-wasm/handler-heap" :refer [heapHelpers ccallMethod]]
            ["ffi-wasm/test-runner" :as tr]))

(defn- fake-module
  "An emscripten-shaped module over one buffer: the eight heap views, a bump
   allocator, getValue/setValue, and the UTF-8 pair. ccall records its calls."
  []
  (let [buffer (js/ArrayBuffer. 1024)
        u8 (js/Uint8Array. buffer)
        bump (atom 8)
        freed (atom [])
        ccalls (atom [])
        m #js {}]
    (set! (.-HEAP8 m) (js/Int8Array. buffer))
    (set! (.-HEAPU8 m) u8)
    (set! (.-HEAP16 m) (js/Int16Array. buffer))
    (set! (.-HEAPU16 m) (js/Uint16Array. buffer))
    (set! (.-HEAP32 m) (js/Int32Array. buffer))
    (set! (.-HEAPU32 m) (js/Uint32Array. buffer))
    (set! (.-HEAPF32 m) (js/Float32Array. buffer))
    (set! (.-HEAPF64 m) (js/Float64Array. buffer))
    (set! (.-_malloc m) (fn [size] (let [p @bump] (swap! bump + (max 8 size)) p)))
    (set! (.-_free m) (fn [ptr] (swap! freed conj ptr) nil))
    ;; "*" is a pointer slot. On wasm32 that is the same 4-byte read as i32,
    ;; and emscripten treats it that way too. read_string_array walks slots
    ;; with "*", so the stand-in has to answer it.
    (set! (.-getValue m) (fn [ptr type]
                           (case type
                             ("i32" "*") (aget (.-HEAP32 m) (bit-shift-right ptr 2))
                             "double" (aget (.-HEAPF64 m) (bit-shift-right ptr 3))
                             (throw (js/Error. (str "fake getValue: " type))))))
    (set! (.-setValue m) (fn [ptr value type]
                           (case type
                             ("i32" "*") (aset (.-HEAP32 m) (bit-shift-right ptr 2) value)
                             "double" (aset (.-HEAPF64 m) (bit-shift-right ptr 3) value)
                             (throw (js/Error. (str "fake setValue: " type))))
                           nil))
    (set! (.-UTF8ToString m) (fn [ptr]
                               (let [end (loop [i ptr] (if (zero? (aget u8 i)) i (recur (inc i))))]
                                 (.decode (js/TextDecoder.) (.subarray u8 ptr end)))))
    (set! (.-stringToUTF8 m) (fn [s ptr _max]
                               (let [bytes (.encode (js/TextEncoder.) s)]
                                 (.set u8 bytes ptr)
                                 (aset u8 (+ ptr (.-length bytes)) 0)
                                 nil)))
    (set! (.-lengthBytesUTF8 m) (fn [s] (.-length (.encode (js/TextEncoder.) s))))
    (set! (.-ccall m) (fn [fn-name ret-type arg-types args]
                        (swap! ccalls conj [fn-name ret-type (vec (js/Array.from arg-types))
                                            (vec (js/Array.from args))])
                        (str "called:" fn-name)))
    {:module m :freed freed :ccalls ccalls :u8 u8}))

(deftest ^:async malloc-and-free-forward-to-the-module
  (let [{:keys [module freed]} (fake-module)
        heap (heapHelpers (fn [] module))
        ptr (await ((.-malloc heap) 16))]
    (is (pos? ptr))
    (is (true? (.-ok (await ((.-free heap) ptr)))))
    (is (= [ptr] @freed))))

(deftest ^:async get-value-and-set-value-round-trip
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))]
    (is (true? (.-ok (await ((.-set_value heap) 16 42 "i32")))))
    (is (= 42 (await ((.-get_value heap) 16 "i32"))))
    (is (true? (.-ok (await ((.-set_value heap) 32 1.5 "double")))))
    (is (= 1.5 (await ((.-get_value heap) 32 "double"))))))

(deftest ^:async the-utf8-trio-round-trips-a-string
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))]
    (is (= 5 (await ((.-utf8_byte_length heap) "hello"))))
    (is (true? (.-ok (await ((.-string_to_utf8 heap) "hello" 64 6)))))
    (is (= "hello" (await ((.-utf8_to_string heap) 64))))))

(deftest ^:async every-heap-view-gets-a-get-and-a-set-pair
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))]
    (testing "all eight prefixes are present as both get and set"
      (doseq [p ["heap8" "heapu8" "heap16" "heapu16" "heap32" "heapu32" "heapf32" "heapf64"]]
        (is (fn? (aget heap (str p "_get"))) (str p "_get"))
        (is (fn? (aget heap (str p "_set"))) (str p "_set"))))
    (testing "a set is readable back through the matching get"
      (await ((.-heapu8_set heap) 100 (js/Uint8Array. #js [1 2 254])))
      (is (= [1 2 254] (vec (js/Array.from (await ((.-heapu8_get heap) 100 3))))))
      (await ((.-heapf64_set heap) 20 (js/Float64Array. #js [2.5 -3.5])))
      (is (= [2.5 -3.5] (vec (js/Array.from (await ((.-heapf64_get heap) 20 2)))))))))

(deftest ^:async a-heap-offset-counts-elements-not-bytes
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))]
    ;; The offset indexes the typed-array view, so the same bytes sit at
    ;; different offsets depending on which view reaches them. Worth pinning:
    ;; a caller reading a wide view at a byte offset gets the wrong window, and
    ;; on a large enough offset a RangeError instead of bad data.
    (await ((.-heapu8_set heap) 8 (js/Uint8Array. #js [1 0 0 0])))
    (is (= [1] (vec (js/Array.from (await ((.-heap32_get heap) 2 1)))))
        "byte 8 is element 2 of HEAP32")
    (testing "an offset past the view's own length is a RangeError, not silence"
      (let [elements (.-length (.-HEAPF64 module))]
        (try
          (await ((.-heapf64_set heap) elements (js/Float64Array. #js [1.0])))
          (is false "should have thrown")
          (catch :default e
            (is (instance? js/RangeError e))))))))

(deftest ^:async a-heap-get-detaches-from-the-wasm-heap
  (let [{:keys [module u8]} (fake-module)
        heap (heapHelpers (fn [] module))]
    (await ((.-heapu8_set heap) 300 (js/Uint8Array. #js [7 7])))
    (let [got (await ((.-heapu8_get heap) 300 2))]
      (testing "the result is a typed array, not a plain array"
        (is (instance? js/Uint8Array got)))
      (aset u8 300 99)
      (is (= [7 7] (vec (js/Array.from got)))
          "a later heap write does not reach the copy, so it survives a free"))))

(deftest ^:async the-shared-ok-result-is-frozen
  ;; Every method with nothing to return answers with one shared object. If a
  ;; caller could write to it, that write would change the answer every later
  ;; call gives. The freeze turns such a write into an error instead.
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))
        r (await ((.-free heap) 8))]
    (is (true? (js/Object.isFrozen r)))
    (is (thrown? js/TypeError (aset r "ok" false)))
    (is (true? (.-ok (await ((.-set_value heap) 16 1 "i32"))))
        "a later call still answers ok")))

(deftest ^:async the-methods-object-late-binds-the-module
  (let [{:keys [module]} (fake-module)
        current (atom nil)
        heap (heapHelpers (fn [] @current))]
    (testing "built before the module exists, it says so rather than crashing oddly"
      (is (thrown-with-msg? js/Error #"getModule\(\) returned null"
                            (await ((.-malloc heap) 8)))))
    (reset! current module)
    (is (pos? (await ((.-malloc heap) 8)))
        "the same object works once init has resolved the module")))

(defn- ^:async write-string-array!
  "Write each string of `strs` into the heap, then a NUL-terminated table of
   pointers to them at `table-ptr`. Returns nothing. The caller picks the
   addresses, so a test can place an entry on purpose."
  [heap table-ptr addrs strs]
  (doseq [[addr s] (map vector addrs strs)]
    (await ((.-string_to_utf8 heap) s addr (inc (count s)))))
  (await ((.-heapu32_set heap)
          (bit-shift-right table-ptr 2)
          (js/Uint32Array. (into-array (conj (vec addrs) 0))))))

(deftest ^:async read-string-array-walks-a-null-terminated-table
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))]
    (await (write-string-array! heap 400 [100 200 300] ["alpha" "beta" "gamma"]))
    (is (= ["alpha" "beta" "gamma"]
           (vec (await ((.-read_string_array heap) 400)))))))

(deftest ^:async read-string-array-gives-an-empty-vector-for-a-null-list
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))]
    (is (= [] (vec (await ((.-read_string_array heap) 0))))
        "a null char** is an empty list, and not a walk from address 0")))

(deftest ^:async an-empty-string-entry-is-not-the-terminator
  ;; The terminator is a NULL SLOT. A slot that points at a lone NUL byte is a
  ;; present, empty string. Confusing the two truncates the list at the first
  ;; empty entry, and the caller never learns that it lost the tail.
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))]
    (await (write-string-array! heap 400 [100 200 300] ["alpha" "" "gamma"]))
    (is (= ["alpha" "" "gamma"]
           (vec (await ((.-read_string_array heap) 400)))))))

(deftest ^:async read-string-array-rejects-a-misaligned-pointer
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))]
    (is (thrown-with-msg? js/Error #"not 4-byte aligned"
                          (await ((.-read_string_array heap) 401))))))

(deftest ^:async an-unterminated-table-throws-instead-of-spinning
  ;; Without the bound this hangs rather than fails, because an out-of-range
  ;; typed-array read gives undefined and undefined is not 0.
  (let [{:keys [module]} (fake-module)
        heap (heapHelpers (fn [] module))
        slots (bit-shift-right (- 1024 512) 2)]
    ;; Byte 4 holds a NUL, so every slot decodes to "" and none terminates.
    (await ((.-heapu32_set heap) (bit-shift-right 512 2)
            (js/Uint32Array. (into-array (repeat slots 4)))))
    (is (thrown-with-msg? js/Error #"walked past the end of the heap"
                          (await ((.-read_string_array heap) 512))))))

(deftest ^:async ccall-method-forwards-its-four-arguments
  (let [{:keys [module ccalls]} (fake-module)
        ccall (ccallMethod (fn [] module))
        result (await (ccall "OGR_L_GetName" "string" #js ["number"] #js [17]))]
    (is (= "called:OGR_L_GetName" result) "the module's return value comes back")
    (is (= [["OGR_L_GetName" "string" ["number"] [17]]] @ccalls)
        "name, return type, arg types and args all arrive unchanged")))

(deftest ^:async ccall-method-late-binds-the-module-too
  (let [ccall (ccallMethod (fn [] nil))]
    (is (thrown-with-msg? js/Error #"getModule\(\) returned null"
                          (await (ccall "AnyFn" "void" #js [] #js []))))))

(tr/run-tests-and-exit! "net.willcohen.native.handler-heap-test")
