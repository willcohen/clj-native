;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.lazy-context-test
  "Pins the laziness of the shared GraalVM Polyglot Context: loading
   dispatch (and through it graal-wasm) must not construct it. A pure-FFI
   consumer should not stand up a wasm engine it never uses, and the
   Context is irrevocable for the life of the JVM once built. An eager
   def regressing here would go unnoticed by every other test, because
   they all force the context themselves.

   The check runs in a subprocess because this JVM's own suites force the
   context. The probe prints the delay's realized? state; renaming either
   var makes the subprocess exit nonzero, so the pin fails loudly instead
   of going quiet."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private probe
  (str "(require 'net.willcohen.native.dispatch)"
       "(print (str \"lazy=\" (and (not (realized? @#'net.willcohen.native.graal-wasm/context-state))"
       " (not (realized? @#'net.willcohen.native.graal-wasm/engine-state)))))"
       "(flush)"))

(deftest loading-dispatch-does-not-construct-the-context
  (let [{:keys [exit out err]} (sh/sh "clojure" "-M" "-e" probe)]
    (testing "the probe subprocess ran"
      (is (= 0 exit) err))
    (testing "the context delay is unrealized after dispatch loads"
      (is (= "lazy=true" out)))))

;; The pooled mirror of the pin above: a pool-only consumer must never
;; force the default Context. A scalar Pointerlike wrap through (context)
;; would realize the singleton (and contend on its monitor) for a
;; consumer that only ever holds pooled Contexts. A host scalar crosses
;; contexts legally, so no in-process assertion can see that defect; the
;; realized? state of the delay can.
(def ^:private pooled-probe
  (str "(require '[net.willcohen.native.graal-wasm :as w])"
       "(let [pctx (w/new-polyglot-context!)"
       "      wc (w/->WasmContext :probe (atom nil))]"
       "  (w/bootstrap-graal-module! wc"
       "    {:loader-module-url (-> (java.io.File. \"test/fixtures/wasm-heap-loader.mjs\") .toURI .toURL)"
       "     :polyglot-context pctx})"
       "  (w/with-wasm-context wc"
       "    (assert (= \"probe\" (w/pointer->string (w/allocate-string-on-heap \"probe\")))))"
       "  (print (str \"default-untouched=\""
       "              (not (realized? @#'net.willcohen.native.graal-wasm/context-state))))"
       "  (flush))"))

(deftest pooled-context-work-does-not-force-the-default-context
  (let [{:keys [exit out err]} (sh/sh "clojure" "-M" "-e" pooled-probe)]
    (testing "the probe subprocess ran"
      (is (= 0 exit) (str err "\n" out)))
    (testing "a pooled bootstrap and string round-trip leave the default Context unrealized"
      (is (str/includes? out "default-untouched=true") out))))
