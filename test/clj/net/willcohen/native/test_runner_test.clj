;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.test-runner-test
  "Pins test_runner.cljc's exit codes and argument contract through
   test/fixtures/exit-code-probe.mjs, which imports the test_runner.mjs that
   bb test:clj compiles first. The driver runs on the JVM, since a squint
   counter rename that false-greens the CLJS suites would false-green a CLJS
   check too."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.shell :as sh]))

(def ^:private probe-path "test/fixtures/exit-code-probe.mjs")

(def ^:private modes ["pass" "fail" "error" "hang" "teardown" "teardown-ns" "teardown-fail"])

;; Each probe is its own node process, so all of them run at once.
(def ^:private runs
  (delay (zipmap modes (pmap #(sh/sh "node" probe-path %) modes))))

(defn- probe [mode]
  (get @runs mode))

(defn- probe-exit [mode]
  (:exit (probe mode)))

(deftest exit-code-tracks-squint-report-counters
  (testing "a failed assertion exits 1 via the \"fail\" counter"
    (is (= 1 (probe-exit "fail"))))
  (testing "a thrown test body exits 1 via the \"error\" counter"
    (is (= 1 (probe-exit "error"))))
  (testing "a test that never settles exits 1 when the event loop drains"
    (is (= 1 (probe-exit "hang")))))

(deftest a-teardown-and-a-namespace-name-are-told-apart-by-type
  (testing "a lone teardown fn runs every registered test, then the teardown"
    (let [{:keys [exit out]} (probe "teardown")]
      (is (= 0 exit))
      (is (re-find #"TEST-RAN" out))
      ;; The teardown resolves on a later tick, so the runner must await it.
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
  (testing "a lone namespace name runs that namespace with no teardown"
    (let [{:keys [exit out]} (probe "pass")]
      (is (= 0 exit))
      (is (re-find #"TEST-RAN" out))
      (is (nil? (re-find #"TEARDOWN-RAN" out))))))
