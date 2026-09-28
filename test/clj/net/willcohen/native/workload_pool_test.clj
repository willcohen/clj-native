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

(defn- await-shutdown!
  "Block until exec has terminated or the timeout elapses, returning a
   boolean."
  [^ExecutorService exec timeout-s]
  (.awaitTermination exec timeout-s TimeUnit/SECONDS))

(deftest current-context-or-nil-is-nil-off-pool-and-state-on-pool
  (testing "off a pool thread it returns nil where current-context throws"
    (is (nil? (wp/current-context-or-nil ::anything)))
    (is (thrown? Exception (wp/current-context ::anything))))
  (testing "on a pool worker it returns the same state current-context does"
    (let [registry (wp/init-workload-pool! {:size 1})]
      (try
        (wp/register-handler! registry :compute ::ccn {:init (fn [_] {:marker 7})})
        (let [^ExecutorService exec (wp/as-executor-service registry :compute)
              got (.get (.submit exec ^Callable
                                 (fn [] [(:marker (wp/current-context-or-nil ::ccn))
                                         (wp/current-context-or-nil ::unregistered)])))]
          (is (= 7 (first got)))
          (is (nil? (second got))))
        (finally
          (wp/shutdown-pool! registry))))))

(deftest init-workload-pool-shape
  (let [registry (wp/init-workload-pool! {:size 2})]
    (is (= 2 (:size registry)))
    (is (= #{:mixed :io :compute}
           (set (keys @(:slots registry)))))
    (is (= #{:mixed :io :compute}
           (set (keys @(:handlers registry)))))
    (is (false? @(:terminated? registry)))
    (wp/shutdown-pool! registry)))

(deftest register-handler-stacks-by-workload
  (let [registry (wp/init-workload-pool! {:size 2})
        init-fn (fn [_] {:state :ready})
        destroy-fn (fn [_])]
    (wp/register-handler! registry :compute :lib-a
                          {:init init-fn :destroy destroy-fn :args nil})
    (wp/register-handler! registry :compute :lib-b
                          {:init init-fn :destroy destroy-fn :args nil})
    (let [compute-handlers (get @(:handlers registry) :compute)]
      (is (= 2 (count compute-handlers)))
      (is (= [:lib-a :lib-b] (mapv :lib-key compute-handlers))))
    (is (empty? (get @(:handlers registry) :mixed)))
    (wp/shutdown-pool! registry)))

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
            results (mapv #(.get %) futures)]
        (is (= 4 (count results)))
        (is (= 4 @init-count) "init runs once per thread (4 threads)")
        (is (= 4 (count (distinct (map :thread results))))
            "every result comes from a distinct thread")
        (wp/shutdown-pool! registry)
        (await-shutdown! exec 5)))))

(deftest current-context-throws-off-pool-thread
  (let [registry (wp/init-workload-pool! {:size 2})]
    (wp/register-handler! registry :compute :probe
                          {:init (fn [_] {:state :ready})
                           :args nil})
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"non-pool thread"
         (wp/current-context :probe))
        "calling current-context from main thread throws")
    (wp/shutdown-pool! registry)))

(deftest current-context-throws-for-unregistered-lib
  (let [registry (wp/init-workload-pool! {:size 1})]
    (wp/register-handler! registry :compute :lib-a
                          {:init (fn [_] {:state :ready})
                           :args nil})
    (let [^ExecutorService exec (wp/as-executor-service registry :compute)
          ex (try
               (.get (.submit exec ^Callable
                              (fn []
                                (wp/current-context :lib-b))))
               (catch java.util.concurrent.ExecutionException e
                 (.getCause e)))]
      (is (instance? clojure.lang.ExceptionInfo ex))
      (is (re-find #"no handler registered" (.getMessage ex)))
      (is (= :lib-b (:lib-key (ex-data ex))))
      (wp/shutdown-pool! registry)
      (await-shutdown! exec 5))))

(deftest as-executor-service-is-idempotent
  (let [registry (wp/init-workload-pool! {:size 1})]
    (wp/register-handler! registry :compute :probe
                          {:init (fn [_] {})
                           :args nil})
    (let [e1 (wp/as-executor-service registry :compute)
          e2 (wp/as-executor-service registry :compute)]
      (is (identical? e1 e2)
          "repeated as-executor-service for the same workload returns the same instance")
      (wp/shutdown-pool! registry))))

(deftest shutdown-fires-destroy
  (testing "Each thread's destroy fires at shutdown for every registered lib"
    (let [destroy-calls (atom [])
          registry (wp/init-workload-pool! {:size 2})]
      (wp/register-handler! registry :compute :probe
                            {:init (fn [_] {:tag :init-state})
                             :destroy (fn [state]
                                        (swap! destroy-calls conj state))
                             :args nil})
      (let [^ExecutorService exec (wp/as-executor-service registry :compute)
            latch (java.util.concurrent.CountDownLatch. 2)
            _ (dotimes [_ 2]
                (.submit exec ^Callable
                         (fn []
                           (.countDown latch)
                           (.await latch 5 TimeUnit/SECONDS)
                           :ok)))
            _ (.await latch 5 TimeUnit/SECONDS)]
        (wp/shutdown-pool! registry)
        (await-shutdown! exec 5)
        (is (= 2 (count @destroy-calls)) "destroy fired on both threads")
        (is (every? (fn [s] (= :init-state (:tag s))) @destroy-calls)
            "destroy received the init state")))))

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
        registry (wp/init-workload-pool! {:size 4})]
    (wp/register-handler! registry :compute :probe
                          {:init    (fn [_] (swap! log conj :init) :state)
                           :destroy (fn [_] (Thread/sleep 200) (swap! log conj :destroy))})
    (let [^ExecutorService exec (wp/as-executor-service registry :compute)]
      (.get (.submit exec ^Callable #(wp/current-context :probe)) 5 TimeUnit/SECONDS)
      (wp/shutdown-pool! registry)
      (is (= [:init :destroy] @log)))))

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
