;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.platform-state-test
  "Tests of platform-state's predicates and force/impl transitions under JVM
   clojure.test and squint cljs.test. The JVM-only try-init! tests supply both
   bootstrap fns, so they need no native or wasm resources."
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is]])
            #?(:clj  [net.willcohen.native.platform-state :as ps]
               :cljs ["ffi-wasm/platform-state" :as ps])
            #?(:cljs ["ffi-wasm/test-runner" :as tr])))

(deftest force-graal!-forces-and-clears
  (let [impl (atom :ffi) force (atom false)]
    (ps/force-graal! impl force)
    (is (true? @force) "force-atom set true")
    (is (nil? @impl) "impl-atom cleared for re-init")))

(deftest force-ffi!-unforces-and-clears
  (let [impl (atom :graal) force (atom true)]
    (ps/force-ffi! impl force)
    (is (false? @force) "force-atom set false")
    (is (nil? @impl) "impl-atom cleared for re-init")))

(deftest toggle-graal!-flips-force-and-clears
  (let [impl (atom :ffi) force (atom false)]
    (ps/toggle-graal! impl force)
    (is (true? @force) "false -> true")
    (is (nil? @impl))
    (reset! impl :graal)
    (ps/toggle-graal! impl force)
    (is (false? @force) "true -> false")
    (is (nil? @impl))))

(deftest null-ptr?-takes-nil-and-0-as-null-on-each-runtime
  (is (ps/null-ptr? nil))
  (is (ps/null-ptr? 0) "a heap read gives 0 for NULL")
  (is (not (ps/null-ptr? 42))))

#?(:clj
   (defn- silently
     "Run `f` with stdout captured and System/err muted, and return its value.
      try-init!'s fallback prints a stack trace that would bury the report."
     [f]
     (let [saved System/err
           result (atom nil)]
       (try
         (System/setErr (java.io.PrintStream. (java.io.ByteArrayOutputStream.)))
         (with-out-str (reset! result (f)))
         (finally (System/setErr saved)))
       @result)))

#?(:clj
   (deftest try-init!-records-ffi-when-the-ffi-bootstrap-succeeds
     (let [impl (atom nil) force (atom false) ran (atom [])]
       (is (= :ffi (silently #(ps/try-init! impl force false
                                            (fn [] (swap! ran conj :ffi))
                                            (fn [] (swap! ran conj :graal))))))
       (is (= :ffi @impl) "the choice is recorded in impl-atom, not only returned")
       (is (= [:ffi] @ran) "the graal bootstrap never ran"))))

#?(:clj
   (deftest try-init!-falls-back-to-graal-on-an-error
     ;; A missing native library throws UnsatisfiedLinkError, which an
     ;; Exception catch would let escape.
     (let [impl (atom nil) force (atom false) ran (atom [])]
       (is (= :graal (silently #(ps/try-init! impl force false
                                              (fn [] (swap! ran conj :ffi)
                                                (throw (UnsatisfiedLinkError. "no such library")))
                                              (fn [] (swap! ran conj :graal))))))
       (is (= :graal @impl))
       (is (= [:ffi :graal] @ran)))))

#?(:clj
   (deftest try-init!-honors-force-graal-without-attempting-ffi
     (let [impl (atom nil) force (atom true) ran (atom [])]
       (is (= :graal (silently #(ps/try-init! impl force false
                                              (fn [] (swap! ran conj :ffi))
                                              (fn [] (swap! ran conj :graal))))))
       (is (= :graal @impl))
       (is (= [:graal] @ran)
           "the ffi path is skipped outright, not attempted and discarded"))))

#?(:cljs (tr/run-tests-and-exit! "net.willcohen.native.platform-state-test"))
