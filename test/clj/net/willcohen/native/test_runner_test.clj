;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.test-runner-test
  "Pins test_runner.cljc's process exit codes against squint's report
   counters. run-tests-and-exit! reads the counter map by the string keys
   \"fail\" and \"error\"; if squint renames either, every CLJS suite in
   clj-native and in clj-proj exits 0 regardless of what failed, and both
   repos' CLJS lanes go silently green.

   The driver lives on the JVM so the verdict comes from the cognitect
   test-runner rather than from the runner under test. A CLJS suite could not
   do this job: the same rename that false-greens the probe would false-green
   the suite asserting on it.

   test/fixtures/exit-code-probe.mjs is the subject; it runs one cljs.test
   namespace under the real compiled test_runner.mjs, in the mode its argument
   names. bb.edn's test:clj task compiles that .mjs first.

   The second group pins the runner's argument contract. run-tests-and-exit!
   tells a teardown fn apart from a namespace name by type, which is what lets
   one consumer pass a shutdown fn and another pass a name. Getting that wrong
   is quiet in both directions: a name treated as a teardown would be called,
   and a teardown treated as a name would leave the process alive."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.shell :as sh]))

(def ^:private probe-path "test/fixtures/exit-code-probe.mjs")

(defn- probe [mode]
  (sh/sh "node" probe-path mode))

(defn- probe-exit [mode]
  (:exit (probe mode)))

(deftest exit-code-tracks-squint-report-counters
  (testing "a green run exits 0"
    (is (= 0 (probe-exit "pass"))))
  (testing "a failed assertion exits 1 via the \"fail\" counter"
    (is (= 1 (probe-exit "fail"))))
  (testing "a thrown test body exits 1 via the \"error\" counter"
    (is (= 1 (probe-exit "error")))))

(deftest a-teardown-and-a-namespace-name-are-told-apart-by-type
  (testing "a lone teardown fn runs every registered test, then the teardown"
    (let [{:keys [exit out]} (probe "teardown")]
      (is (= 0 exit))
      (is (re-find #"TEST-RAN" out))
      ;; The teardown resolves on a later tick, so this line is here only
      ;; because the runner awaited it before exiting.
      (is (re-find #"TEARDOWN-RAN" out))))
  (testing "a teardown followed by a namespace name runs both"
    (let [{:keys [exit out]} (probe "teardown-ns")]
      (is (= 0 exit))
      (is (re-find #"TEST-RAN" out))
      (is (re-find #"TEARDOWN-RAN" out))))
  (testing "a failing run still runs the teardown, and still exits 1"
    (let [{:keys [exit out]} (probe "teardown-fail")]
      (is (= 1 exit))
      (is (re-find #"TEARDOWN-RAN" out))))
  (testing "a lone namespace name is a name, not a teardown to call"
    (let [{:keys [exit out]} (probe "pass")]
      (is (= 0 exit))
      (is (re-find #"TEST-RAN" out))
      (is (nil? (re-find #"TEARDOWN-RAN" out))))))
