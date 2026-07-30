;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.handler-env-test
  "Coverage for handler-env's environment classifier.

   classifyEnvironment takes the globals object as an argument, so every
   branch runs against a stand-in and the suite never mutates the real
   globals. detectEnvironment and the two captured booleans are then checked
   against the runtime the suite actually runs in, which is Node."
  (:require [cljs.test :refer [deftest is testing]]
            ["ffi-wasm/handler-env"
             :refer [classifyEnvironment detectEnvironment isNode isBrowser]]
            ["ffi-wasm/test-runner" :as tr]))

(deftest classify-recognizes-node-by-process-versions-node
  (is (= "node" (classifyEnvironment #js {:process #js {:versions #js {:node "26.5.0"}}})))
  (testing "a process without a versions.node is not Node"
    (is (= "unknown" (classifyEnvironment #js {:process #js {:versions #js {}}})))
    (is (= "unknown" (classifyEnvironment #js {:process #js {}})))))

(deftest classify-recognizes-a-browser-by-window-document
  (is (= "browser" (classifyEnvironment #js {:window #js {:document #js {}}})))
  (testing "a window without a document is not a browser"
    (is (= "unknown" (classifyEnvironment #js {:window #js {}})))))

(deftest classify-calls-a-web-worker-unknown
  (testing "a worker has neither process nor window, so neither name fits"
    (is (= "unknown" (classifyEnvironment #js {:self #js {}})))
    (is (= "unknown" (classifyEnvironment #js {})))
    (is (= "unknown" (classifyEnvironment nil)))))

(deftest classify-prefers-node-when-both-look-present
  (testing "an electron-shaped globals object classifies as node, not browser"
    (is (= "node" (classifyEnvironment #js {:process #js {:versions #js {:node "26.5.0"}}
                                            :window #js {:document #js {}}})))))

(deftest detect-and-the-captured-booleans-agree-on-this-runtime
  (is (= "node" (detectEnvironment)) "the suite runs on Node")
  (is (true? isNode) "isNode captures the same answer at import")
  (is (false? isBrowser)))

(tr/run-tests-and-exit! "net.willcohen.native.handler-env-test")
