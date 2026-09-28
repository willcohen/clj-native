;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.lazy-context-test
  "Checks that loading dispatch, and heap work on a pooled Context, do not
   build the shared GraalVM Context, which lasts for the life of the JVM.
   The probe runs in a subprocess, since this JVM's other suites build it."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

;; A host scalar crosses Contexts legally, so only realized? can see a
;; pooled call that goes through the shared Context.
(def ^:private probe
  (str "(require 'net.willcohen.native.dispatch '[net.willcohen.native.graal-wasm :as w])"
       "(defn unbuilt? [] (not (realized? @#'w/context-state)))"
       "(print (str \"lazy=\" (and (unbuilt?) (not (realized? @#'w/engine-state)))))"
       "(let [wc (w/->WasmContext :probe (atom nil))]"
       "  (w/bootstrap-graal-module! wc"
       "    {:loader-module-url (-> (java.io.File. \"test/fixtures/wasm-heap-loader.mjs\") .toURI .toURL)"
       "     :polyglot-context (w/new-polyglot-context!)})"
       "  (w/with-wasm-context wc"
       "    (assert (= [\"probe\"] (w/string-array-pointer->strs (w/string-list-to-native-array [\"probe\"]))))))"
       "(print (str \" pooled-untouched=\" (unbuilt?)))"
       "(flush)"))

(deftest dispatch-and-pooled-work-leave-the-shared-context-unbuilt
  (let [{:keys [exit out err]} (sh/sh "clojure" "-M" "-e" probe)]
    (is (= 0 exit) (str err "\n" out))
    (is (str/includes? out "lazy=true") out)
    (is (str/includes? out "pooled-untouched=true") out)))
