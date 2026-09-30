;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.bundle-test
  "ffi-wasm ships one page bundle and one worker bundle. One file is one
   module instance, so each consumer of ffi-wasm shares the state of pool."
  (:require [cljs.test :refer [deftest is]]
            ["ffi-wasm/test-runner" :as tr]
            ["node:fs" :refer [existsSync readFileSync]]
            ["node:path" :refer [dirname join]]
            ["node:url" :refer [fileURLToPath]]))

(def ^:private dist-dir
  (dirname (fileURLToPath (.resolve js/import.meta "ffi-wasm"))))

(def ^:private page-modules
  ["pool" "dispatch" "platform_state" "macros" "workload_pool" "handler_runtime"
   "handler_env" "handler_fs" "handler_heap" "handler_paths" "http_bridge"])

(defn- src-module-url [m]
  (.-href (js/URL. (str "../../../../../src/cljc/net/willcohen/native/" m ".mjs")
                   (.-url js/import.meta))))

;; A dynamic import gives the same namespace object as a static import.
(defn- ^:async import-ns [spec]
  (await (js/import (.resolve js/import.meta spec))))

(deftest ^:async every-page-subpath-is-the-page-bundle
  (let [ffi (await (import-ns "ffi-wasm"))]
    (doseq [spec ["ffi-wasm/pool" "ffi-wasm/dispatch" "ffi-wasm/workload-pool"
                  "ffi-wasm/handler-runtime"]]
      (is (identical? ffi (await (import-ns spec))) spec))
    (is (not (identical? ffi (await (import-ns "ffi-wasm/test-runner"))))
        "the test runner is a file of its own")))

(deftest ^:async the-page-bundle-exports-every-name-of-its-modules
  ;; export * drops a name that two modules export, with no error.
  (let [ffi (await (import-ns "ffi-wasm"))]
    (doseq [m page-modules]
      (let [mod (await (js/import (src-module-url m)))]
        (doseq [k (js/Object.keys mod)]
          (is (js/Object.hasOwn ffi k) (str m " exports " k)))))))

(deftest ^:async the-worker-bundle-holds-the-worker-side
  (let [ffi (await (import-ns "ffi-wasm"))
        h   (await (import-ns "ffi-wasm/handler"))]
    (is (fn? (.-makeHandler h)))
    (is (fn? (.-stageFiles h)))
    (is (fn? (.-createSyncFetch h)))
    (is (not (identical? ffi h)) "the worker bundle is a file of its own")))

;; init-pool! and createSyncFetch find these next to the bundle.
(deftest the-bundle-files-sit-together
  (doseq [f ["handler.mjs" "fetch_worker.mjs" "test_runner.mjs"]]
    (is (existsSync (join dist-dir f)) f)))

;; Each library ships its own code only. A page loads worker-router and
;; comlink from their own packages, one copy each.
(deftest dist-holds-no-copy-of-another-library
  (doseq [f ["worker-bootstrap.mjs" "comlink.mjs"]]
    (is (not (existsSync (join dist-dir f))) f)))

;; The bundle holds ffi-wasm's own modules only. A copy of another package
;; is a second module instance next to the copy that a consumer imports.
(deftest the-bundles-inline-no-other-package
  (doseq [f ["ffi-wasm.mjs" "handler.mjs"]
          :let [text (readFileSync (join dist-dir f) "utf8")]]
    (is (not (.includes text "// node_modules/")) f))
  (let [text (readFileSync (join dist-dir "ffi-wasm.mjs") "utf8")]
    (is (.includes text "from \"squint-cljs/core.js\""))
    (is (.includes text "from \"worker-router\""))
    (is (not (.includes text "squint-cljs/src/squint/test.js"))
        "a page maps no importmap entry for the squint test library")))

;; A consumer that bundles a dist file marks node:* as external, so a Node
;; builtin with no node: prefix fails its build.
(deftest the-dist-files-import-each-node-builtin-with-the-node-prefix
  (doseq [f ["ffi-wasm.mjs" "handler.mjs" "fetch_worker.mjs"]
          :let [text (readFileSync (join dist-dir f) "utf8")]
          b ["worker_threads" "fs" "path" "url"]
          q ["\"" "'"]
          form [(str "import(" q b q ")") (str "from " q b q)]]
    (is (not (.includes text form)) (str f ": " form))))

(tr/run-tests-and-exit! "net.willcohen.native.bundle-test")
