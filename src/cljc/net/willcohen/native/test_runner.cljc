;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.test-runner
  "cljs.test runner footer for clj-native consumers.

   The clj-native worker pool keeps the Node event loop alive after
   cljs.test/run-tests resolves. Thus a `(when (pos? ...) (.exit 1))`
   footer hangs on success. A bb timeout masks that hang as exit 124.
   The helper here always exits: 0 when all tests pass, and 1 on any
   failure or error. JVM consumers ignore this namespace, because the
   cognitect test-runner already does the process exit.

   The optional first-argument teardown is for consumers with their own
   runtime state. That state stays alive after cljs.test/run-tests
   resolves. A worker pool shutdown and custom watchers are two examples.

   Squint ^:async metadata does not propagate to inline functions in
   argument position. Thus the teardown must be a top-level ^:async
   defn, or any function that returns a Promise. The inner await then
   compiles correctly.

   Usage at the bottom of a cljs.test mirror:

     (ns my-test
       (:require #?(:cljs [cljs.test :as t])
                 #?(:cljs [net.willcohen.native.test-runner :as tr])))

     #?(:cljs (tr/run-tests-and-exit! \"my-test\"))

   With teardown (for example, a library's worker pool):

     (defn ^:async shutdown! [] (await (.shutdown mylib)))
     #?(:cljs (tr/run-tests-and-exit! shutdown! \"my-test\"))"
  #?(:cljs (:require [cljs.test :as cljs-test])))

#?(:cljs
   (defn run-tests-and-exit!
     "Invoke cljs.test/run-tests for the given namespace name strings.
      With zero arguments, this runs all registered tests. Await any
      returned Promise. Then call process.exit with 0 when all tests
      pass, or 1 on any failure or error.

      An optional teardown function comes FIRST. It takes zero
      arguments, and it is either synchronous or returns a Promise.
      Every argument after the teardown is a namespace name.
      run-tests-and-exit! separates the two by type, and the argument
      count does not matter. Thus the two calls below are each
      complete:

        (run-tests-and-exit! shutdown!)      ; teardown, all tests
        (run-tests-and-exit! \"my.ns-test\")   ; one namespace, no teardown

      The teardown runs after run-tests resolves and before
      process.exit. Use the teardown when the consumer process keeps
      the event loop alive. Worker pools and file-system watchers are
      two examples, and an explicit shutdown is necessary for them.

      cljs.test/run-tests returns a synchronous result map when no test
      is ^:async. If at least one test is ^:async, it returns a Promise
      that resolves to a result map. Promise.resolve normalizes the two
      paths."
     [& args]
     (let [first-arg (first args)
           teardown  (when (fn? first-arg) first-arg)
           ns-names  (if teardown (rest args) args)]
       (-> (js/Promise.resolve (apply cljs-test/run-tests ns-names))
           (.then (fn [results]
                    (let [fail      (or (get results "fail") 0)
                          err       (or (get results "error") 0)
                          exit-code (if (pos? (+ fail err)) 1 0)]
                      (if teardown
                        (.then (js/Promise.resolve (teardown))
                               (fn [_] (.exit js/process exit-code)))
                        (.exit js/process exit-code)))))
           (.catch (fn [e]
                     (js/console.error
                      "clj-native test-runner: test run or teardown rejected; exiting 1:" e)
                     (.exit js/process 1)))))))
