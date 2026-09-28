;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.test-runner
  "cljs.test runner that always exits the process.

   The clj-native worker pool keeps the Node event loop alive after
   cljs.test/run-tests resolves, so a runner that exits only on failure
   hangs on success. JVM consumers do not need this namespace.

     (ns my-test
       (:require #?(:cljs [cljs.test :as t])
                 #?(:cljs [\"ffi-wasm/test-runner\" :as tr])))

     #?(:cljs (tr/run-tests-and-exit! \"my-test\"))

   With a teardown:

     (defn ^:async shutdown! [] (await (.shutdown mylib)))
     #?(:cljs (tr/run-tests-and-exit! shutdown! \"my-test\"))"
  #?(:cljs (:require [cljs.test :as cljs-test])))

#?(:cljs
   (defn run-tests-and-exit!
     "Run cljs.test/run-tests on the named namespaces, or on all with no
      names. Exit 0 when all pass, and 1 on a failure, an error, a rejection,
      or an event loop that drains before the run ends.

      An optional zero-argument teardown fn comes first. It runs before
      the exit and can return a Promise."
     [& args]
     ;; process.exit emits no beforeExit, so this runs only when the event
     ;; loop drains first: a test Promise that never settled.
     (.once js/process "beforeExit"
            (fn [_]
              (js/console.error "clj-native test-runner: the event loop drained before the tests ended; exiting 1")
              (.exit js/process 1)))
     (let [first-arg (first args)
           teardown  (when (fn? first-arg) first-arg)
           ns-names  (if teardown (rest args) args)]
       (-> (js/Promise.resolve (apply cljs-test/run-tests ns-names))
           (.then (fn [results]
                    (let [fail      (or (get results "fail") 0)
                          err       (or (get results "error") 0)
                          exit-code (if (pos? (+ fail err)) 1 0)]
                      (.then (js/Promise.resolve (when teardown (teardown)))
                             (fn [_] (.exit js/process exit-code))))))
           (.catch (fn [e]
                     (js/console.error
                      "clj-native test-runner: test run or teardown rejected; exiting 1:" e)
                     (.exit js/process 1)))))))
