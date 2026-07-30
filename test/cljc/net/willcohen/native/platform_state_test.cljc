;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.platform-state-test
  "Dual-runtime coverage for platform-state's pure predicates and the
   two-atom force/impl transitions. Every fn takes atoms or plain values
   as arguments, so one body exercises identical logic under JVM
   clojure.test and squint cljs.test. The one genuine platform
   divergence -- null-ptr? treating 0 as null on cljs/wasm but as a
   valid pointer on JVM dt-ffi -- is pinned behind a reader conditional.

   try-init! is JVM-only, so its branch tests sit under :clj. Both
   bootstrap fns are supplied by the test, so all three branches run on a
   clean checkout with no native or wasm resources present."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
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

(deftest null-ptr?-and-some-ptr?-are-complementary
  (testing "nil is the null pointer in both runtimes"
    (is (ps/null-ptr? nil))
    (is (not (ps/some-ptr? nil))))
  (testing "a non-zero pointer is non-null in both runtimes"
    (is (not (ps/null-ptr? 42)))
    (is (ps/some-ptr? 42)))
  (testing "0 diverges: null on cljs/wasm, a valid pointer on JVM dt-ffi"
    #?(:clj  (do (is (not (ps/null-ptr? 0)) "JVM: 0 is a valid pointer")
                 (is (ps/some-ptr? 0)))
       :cljs (do (is (ps/null-ptr? 0) "cljs/wasm: 0 is the null pointer")
                 (is (not (ps/some-ptr? 0)))))))

#?(:clj
   (defn- silently
     "Run f with stdout captured and System/err muted; returns f's value.
      try-init!'s fallback branch prints a diagnostic block and a stack trace
      unconditionally, which would otherwise bury the test report."
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
   (deftest try-init!-falls-back-to-graal-when-the-ffi-bootstrap-throws
     (let [impl (atom nil) force (atom false) ran (atom [])]
       (is (= :graal (silently #(ps/try-init! impl force false
                                              (fn [] (swap! ran conj :ffi)
                                                (throw (ex-info "no ffi here" {})))
                                              (fn [] (swap! ran conj :graal))))))
       (is (= :graal @impl))
       (is (= [:ffi :graal] @ran) "ffi is attempted first, then graal picks up"))))

#?(:clj
   (deftest try-init!-falls-back-on-an-error-not-just-an-exception
     ;; The catch is Throwable on purpose: a missing or incompatible native
     ;; library surfaces as UnsatisfiedLinkError, which an Exception catch
     ;; would let escape and take the whole init down instead of falling back.
     (let [impl (atom nil) force (atom false) ran (atom [])]
       (is (= :graal (silently #(ps/try-init! impl force false
                                              (fn [] (throw (UnsatisfiedLinkError. "no such library")))
                                              (fn [] (swap! ran conj :graal))))))
       (is (= :graal @impl))
       (is (= [:graal] @ran)))))

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
