;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.workload-pool-test
  "JVM smoke tests for the workload-pool registry."
  (:require [clojure.test :refer [deftest is testing]]
            [net.willcohen.native.workload-pool :as wp])
  (:import [java.util.concurrent ExecutorService Callable TimeUnit]))

(deftest current-context-or-nil-is-nil-off-pool-and-state-on-pool
  (testing "off a pool thread it returns nil where current-context throws"
    (is (nil? (wp/current-context-or-nil ::anything)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"non-pool thread"
                          (wp/current-context ::anything))))
  (testing "on a pool worker it returns the same state current-context does"
    (let [registry (wp/init-workload-pool! {:size 1})]
      (try
        (wp/register-handler! registry :compute ::ccn {:init (fn [_] {:marker 7})})
        (let [^ExecutorService exec (wp/as-executor-service registry :compute)
              got (.get (.submit exec ^Callable
                                 (fn [] [(:marker (wp/current-context-or-nil ::ccn))
                                         (wp/current-context-or-nil ::unregistered)]))
                        5 TimeUnit/SECONDS)]
          (is (= 7 (first got)))
          (is (nil? (second got))))
        (finally
          (wp/shutdown-pool! registry))))))

(deftest as-executor-service-runs-init-per-thread
  (testing "Each worker thread fires every registered init exactly once"
    (let [init-count (atom 0)
          registry (wp/init-workload-pool! {:size 4})]
      (wp/register-handler! registry :compute :probe
                            {:init (fn [_]
                                     (swap! init-count inc)
                                     {:thread (.getName (Thread/currentThread))})
                             :destroy (fn [_])
                             :args nil})
      (let [^ExecutorService exec (wp/as-executor-service registry :compute)
            ;; The latch holds all 4 tasks in flight, so the executor spawns 4 threads.
            latch (java.util.concurrent.CountDownLatch. 4)
            futures (vec (for [_ (range 4)]
                           (.submit exec
                                    ^Callable
                                    (fn []
                                      (.countDown latch)
                                      (.await latch 5 TimeUnit/SECONDS)
                                      (wp/current-context :probe)))))
            results (mapv #(.get ^java.util.concurrent.Future % 5 TimeUnit/SECONDS) futures)]
        (is (= 4 @init-count) "init runs once per thread (4 threads)")
        (is (= 4 (count (distinct (map :thread results))))
            "every result comes from a distinct thread")
        (is (identical? exec (wp/as-executor-service registry :compute))
            "a second call returns the same executor")
        (wp/shutdown-pool! registry)))))

(deftest current-context-throws-for-unregistered-lib
  (let [registry (wp/init-workload-pool! {:size 1})]
    (wp/register-handler! registry :compute :lib-a
                          {:init (fn [_] {:state :ready})
                           :args nil})
    (let [^ExecutorService exec (wp/as-executor-service registry :compute)
          ex (try
               (.get (.submit exec ^Callable
                              (fn []
                                (wp/current-context :lib-b)))
                     5 TimeUnit/SECONDS)
               (catch java.util.concurrent.ExecutionException e
                 (.getCause e)))]
      (is (instance? clojure.lang.ExceptionInfo ex))
      (is (re-find #"no handler registered" (.getMessage ex)))
      (is (= :lib-b (:lib-key (ex-data ex))))
      (wp/shutdown-pool! registry))))

(deftest release-once!-runs-once-across-threads
  (let [pointer (Object.)
        runs    (atom 0)
        start   (java.util.concurrent.CountDownLatch. 1)
        calls   (doall (repeatedly 8 #(future (.await start)
                                              (wp/release-once! pointer nil (fn [] (swap! runs inc))))))]
    (.countDown start)
    (is (= [true] (filterv some? (map deref calls))))
    (is (= 1 @runs))))

(deftest a-handler-init-error-fails-its-task-and-not-the-pool
  (let [calls    (atom 0)
        registry (wp/init-workload-pool! {:size 1})]
    (wp/register-handler! registry :compute :probe
                          {:init (fn [_]
                                   (if (= 1 (swap! calls inc))
                                     (throw (ex-info "init failed" {}))
                                     {:ok true}))})
    (let [^ExecutorService exec (wp/as-executor-service registry :compute)
          task ^Callable #(wp/current-context :probe)]
      (try
        (let [cause (try (.get (.submit exec task) 5 TimeUnit/SECONDS)
                         (catch java.util.concurrent.ExecutionException e (.getCause e)))]
          (is (= "init failed" (ex-message cause))))
        (is (= {:ok true} (.get (.submit exec task) 5 TimeUnit/SECONDS))
            "the next task on the same thread tries the init again")
        (finally (wp/shutdown-pool! registry))))))

(deftest a-futuretask-passed-to-execute-runs-the-handler-init
  ;; core.async.flow futurize wraps each step in its own FutureTask and calls .execute.
  (let [registry (wp/init-workload-pool! {:size 1})]
    (wp/register-handler! registry :compute :probe {:init (fn [_] :state)})
    (let [^ExecutorService exec (wp/as-executor-service registry :compute)
          task (java.util.concurrent.FutureTask. ^Callable #(wp/current-context :probe))]
      (try
        (.execute exec task)
        (is (= :state (.get task 5 TimeUnit/SECONDS)))
        (finally (wp/shutdown-pool! registry))))))

(deftest shutdown-starts-no-thread-and-waits-for-each-destroy
  (let [log      (atom [])
        registry (wp/init-workload-pool! {:size 4})
        spec     (fn [k] {:init    (fn [_] (swap! log conj [:init k]) k)
                          :destroy (fn [s] (Thread/sleep 10) (swap! log conj [:destroy s]))})]
    (wp/register-handler! registry :compute :a (spec :a))
    (wp/register-handler! registry :compute :b (spec :b))
    (let [^ExecutorService exec (wp/as-executor-service registry :compute)]
      (.get (.submit exec ^Callable #(wp/current-context :a)) 5 TimeUnit/SECONDS)
      (wp/shutdown-pool! registry)
      (is (= [[:init :a] [:init :b] [:destroy :b] [:destroy :a]] @log)
          "one thread, its destroys in reverse order on their init state"))))

(deftest a-second-registration-of-a-lib-key-replaces-the-first
  (let [log      (atom [])
        registry (wp/init-workload-pool! {:size 1})
        spec     (fn [tag] {:init    (fn [_] (swap! log conj [:init tag]) tag)
                            :destroy (fn [s] (swap! log conj [:destroy tag s]))})]
    (wp/register-handler! registry :compute :lib (spec :first))
    (wp/register-handler! registry :compute :lib (spec :second))
    (let [^ExecutorService exec (wp/as-executor-service registry :compute)]
      (.get (.submit exec ^Callable #(wp/current-context :lib)) 5 TimeUnit/SECONDS)
      (wp/shutdown-pool! registry)
      (is (= [[:init :second] [:destroy :second :second]] @log)))))
