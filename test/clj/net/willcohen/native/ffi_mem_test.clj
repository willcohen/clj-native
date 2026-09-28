;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.ffi-mem-test
  "JVM round-trip tests for the native-memory primitives, on dtype native
   buffers. They need no external native library."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [net.willcohen.native.ffi-mem :as m]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as dt-nb]
            [tech.v3.resource :as resource]))

(use-fixtures :once (fn [f] (dt-ffi/set-ffi-impl! :jdk) (f)))

;; A buffer that scratch drops is GC-tracked outside a stack context, so a GC
;; could free it while the test still writes through its address.
(use-fixtures :each (fn [f] (resource/stack-resource-context (f))))

(defn- scratch ^long [n]
  (m/ptr-addr (dt-nb/malloc (long n) {:datatype :int8})))

(deftest scalar-round-trip
  (testing "put-i32!/put-i64!/put-byte! read back through rd-i32/rd-addr"
    (let [a (scratch 64)]
      (m/put-i32! a 0 12345)
      (m/put-i64! a 8 9999999999)
      (m/put-byte! a 16 65)
      (m/put-ptr! a 24 0xdeadbeef)
      (is (= 12345 (m/rd-i32 a)))
      (is (= 9999999999 (m/rd-addr (+ a 8))))
      (is (= 65 (m/rd-i32 (+ a 16))))
      (is (= 0xdeadbeef (m/rd-addr (+ a 24))))))
  (testing "rd-f64 reads a native double"
    (let [a (scratch 8)]
      (.putDouble ^sun.misc.Unsafe (dt-nb/unsafe) a 3.5)
      (is (= 3.5 (m/rd-f64 a))))))

(deftest copy-bytes-round-trip
  (let [a (scratch 16)
        n (m/copy-bytes! a (byte-array [10 20 30 40]))]
    (is (= 4 n))
    (is (= [10 20 30 40] (mapv #(bit-and (m/rd-i32 (+ a %)) 0xFF) [0 1 2 3])))
    (testing "empty array is a no-op returning 0"
      (is (= 0 (m/copy-bytes! a (byte-array 0)))))))

(deftest strings->c-array-round-trip
  (testing "self-contained char** round-trips through read-string-array"
    (is (= ["alpha" "beta" "gamma"]
           (m/read-string-array (m/ptr-addr (m/strings->c-array ["alpha" "beta" "gamma"])))))
    (is (= [] (m/read-string-array (m/ptr-addr (m/strings->c-array []))))))
  (testing "read-string-array on a NULL base is empty"
    (is (= [] (m/read-string-array 0)))))

(deftest ptrs->native-array-round-trip
  (let [a (m/ptr-addr (m/ptrs->native-array [(dt-ffi/->pointer 111) (dt-ffi/->pointer 222)]))]
    (is (= 111 (m/rd-addr a)))
    (is (= 222 (m/rd-addr (+ a 8))))))

(deftest alloc-cstring-round-trip
  ;; Fill with 0xFF, since a zeroed block from dt-nb/malloc hides a missing
  ;; NUL. The 4 spare bytes keep the 4-byte rd-i32 probe inside the block.
  (let [alloc (fn [n]
                (let [a (scratch (+ n 4))]
                  (dotimes [i (+ n 4)] (m/put-byte! a i -1))
                  a))
        addr (m/alloc-cstring alloc "hello")
        slot (scratch 8)]
    (m/put-ptr! slot 0 addr)
    (is (= "hello" (m/rd-cstr slot)))
    (testing "NUL terminator lands right after the payload"
      (is (= 0 (bit-and (m/rd-i32 (+ addr 5)) 0xFF))))
    (testing "nil string allocates nothing and returns address 0"
      (is (= 0 (m/alloc-cstring alloc nil))))))
