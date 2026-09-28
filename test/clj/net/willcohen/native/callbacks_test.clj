;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.callbacks-test
  "JVM smoke tests for upcall registration under the :jdk backend. The consumer
   suites (clj-proj, clj-gdal) test callback invocation end to end."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [net.willcohen.native.callbacks :as cb]
            [net.willcohen.native.ffi-mem :as m]
            [tech.v3.datatype.ffi :as dt-ffi])
  (:import [tech.v3.datatype.ffi Pointer]))

(use-fixtures :once (fn [f] (dt-ffi/set-ffi-impl! :jdk) (f)))

(deftest register-callback-produces-retained-pointer
  (let [iface (cb/define-callback-interface :int64 [:int64])
        {:keys [ptr inst]} (cb/register-callback! iface (fn [x] (* 2 (long x))))]
    (is (instance? Pointer ptr))
    (is (pos? (m/ptr-addr ptr)) "upcall stub has a real native address")
    (is (some? inst) "instance is retained so the caller can keep it GC-alive")))

(deftest repeated-registration-yields-distinct-instances
  (testing "a cached iface can back multiple independent callbacks"
    (let [iface (cb/define-callback-interface :void [:pointer :int32 :pointer])
          a (cb/register-callback! iface (fn [_ _ _] nil))
          b (cb/register-callback! iface (fn [_ _ _] nil))]
      (is (not (identical? (:inst a) (:inst b))))
      (is (not= (m/ptr-addr (:ptr a)) (m/ptr-addr (:ptr b)))))))
