;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.callbacks-test
  "JVM tests for upcall registration under the :jdk backend. They call each
   stub back through a Panama downcall."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [net.willcohen.native.callbacks :as cb]
            [net.willcohen.native.ffi-mem :as m]
            [tech.v3.datatype.ffi :as dt-ffi])
  (:import [java.lang.foreign FunctionDescriptor Linker Linker$Option MemoryLayout
            MemorySegment ValueLayout]
           [java.lang.ref Reference]))

(use-fixtures :once (fn [f] (dt-ffi/set-ffi-impl! :jdk) (f)))

(defn- call-long->long
  "Call the C fn long(long) at the address of `ptr` with `x`."
  [ptr x]
  (let [l  ValueLayout/JAVA_LONG
        mh (.downcallHandle (Linker/nativeLinker)
                            (MemorySegment/ofAddress (m/ptr-addr ptr))
                            (FunctionDescriptor/of l (into-array MemoryLayout [l]))
                            (into-array Linker$Option []))]
    (.invokeWithArguments mh (object-array [x]))))

(deftest each-registered-callback-runs-its-own-fn
  ;; One cached iface backs both callbacks.
  (let [iface (cb/define-callback-interface :int64 [:int64])
        a     (cb/register-callback! iface (fn [x] (* 2 (long x))))
        b     (cb/register-callback! iface (fn [x] (* 3 (long x))))]
    (is (= [42 63] [(call-long->long (:ptr a) 21) (call-long->long (:ptr b) 21)]))
    ;; A collected :inst frees its stub, so hold both until the calls return.
    (Reference/reachabilityFence [a b])))
