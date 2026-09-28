;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.gen-handler-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [net.willcohen.native.gen-handler :as g]))

(def sample-fndefs
  {:mylib_create
   {:rettype :pointer
    :argtypes [[:context :pointer] [:definition :string]]}
   :mylib_destroy
   {:rettype :void
    :argtypes [[:pj :pointer]]
    :destroy? true}
   :mylib_context_destroy
   {:rettype :void
    :argtypes [[:context :pointer]]
    :destroy? true}
   :mylib_string_list_destroy
   {:rettype :void
    :argtypes [[:list :pointer]]
    :destroy? true}})

(def mylib-overrides
  {:overrides-import-path "./mylib-handler-overrides.mjs"
   :runtime-import-path "ffi-wasm/handler-runtime"
   :exposed-methods [:context_create :set_log_level :context_destroy
                     :ccall :malloc :free
                     :heapf64_set :heapf64_get :read_string_array
                     :heapu8_set :heapu8_get
                     :string_to_utf8 :utf8_to_string
                     :shutdown]
   :busy-methods [:ccall :malloc :context_create :set_log_level
                  :heapf64_set :heapf64_get :read_string_array
                  :heapu8_set :heapu8_get
                  :string_to_utf8 :utf8_to_string]
   :destroy-methods [:context_destroy :free :shutdown]
   :fingerprint-fields [:dbBytes]})

(defn- destroy-block [source]
  (let [start (str/index-of source "const destroyFns =")
        end   (str/index-of source ";\n" start)]
    (subs source start end)))

(deftest gen-handler-source--imports
  (let [source (g/gen-handler-source sample-fndefs mylib-overrides)]
    (is (str/includes? source "import { makeHandler, byteLengthFingerprint } from 'ffi-wasm/handler-runtime'"))
    (is (str/includes? source "import * as overrides from './mylib-handler-overrides.mjs'"))))

(deftest gen-handler-source--classification-arrays
  (let [source (g/gen-handler-source {} mylib-overrides)]
    (is (str/includes? source "const busyMethods ="))
    (is (str/includes? source "\"ccall\""))
    (is (str/includes? source "\"malloc\""))
    (is (str/includes? source "const destroyMethods ="))
    (is (str/includes? source "\"context_destroy\""))
    (is (str/includes? source "\"free\""))
    (is (str/includes? source "\"shutdown\""))))

(deftest gen-handler-source--destroy-fns-from-fndefs
  (testing "When :destroy-fns is omitted, derived from fndefs :destroy?"
    (let [source (g/gen-handler-source sample-fndefs mylib-overrides)
          block  (destroy-block source)]
      (is (str/includes? block "\"mylib_destroy\""))
      (is (str/includes? block "\"mylib_context_destroy\""))
      (is (str/includes? block "\"mylib_string_list_destroy\""))
      (is (not (str/includes? block "\"mylib_create\""))))))

(deftest gen-handler-source--explicit-destroy-fns-override-fndefs
  (let [overrides (assoc mylib-overrides :destroy-fns ["custom_destroy_fn"])
        source (g/gen-handler-source sample-fndefs overrides)
        block  (destroy-block source)]
    (is (str/includes? block "\"custom_destroy_fn\""))
    (is (not (str/includes? block "\"mylib_destroy\"")))))

(deftest gen-handler-source--exposed-methods-object
  (let [source (g/gen-handler-source sample-fndefs mylib-overrides)]
    (is (str/includes? source "const methods = {"))
    (doseq [m [:context_create :ccall :malloc :free :shutdown :heapf64_set]]
      (is (str/includes? source (str (name m) ": overrides.methods." (name m)))
          (str "method " (name m) " missing from emitted methods object")))))

;; worker-router's worker-bootstrap gates teardown on
;; `typeof mod.destroy === 'function'`.
(deftest gen-handler-source--destroy-export
  (let [source (g/gen-handler-source sample-fndefs mylib-overrides)]
    (is (str/includes? source "export const destroy = overrides.destroy;"))))

(deftest gen-handler-source--required-keys
  (testing "Missing :overrides-import-path throws"
    (is (thrown? Exception
                 (g/gen-handler-source
                  {} (dissoc mylib-overrides :overrides-import-path)))))
  (testing "Missing :runtime-import-path throws"
    (is (thrown? Exception
                 (g/gen-handler-source
                  {} (dissoc mylib-overrides :runtime-import-path)))))
  (testing "Missing :exposed-methods throws"
    (is (thrown? Exception
                 (g/gen-handler-source
                  {} (dissoc mylib-overrides :exposed-methods)))))
  (testing "Missing :fingerprint-fields throws"
    (is (thrown? Exception
                 (g/gen-handler-source
                  {} (dissoc mylib-overrides :fingerprint-fields))))))

(deftest gen-handler-source--classification-is-optional
  ;; The runtime queue is serial, so the classification affects trace fields only.
  (let [source (g/gen-handler-source
                sample-fndefs
                (dissoc mylib-overrides :busy-methods :destroy-methods))]
    (is (str/includes? source "const busyMethods = [];"))
    (is (str/includes? source "const destroyMethods = [];"))))

(defn- node-on-path? []
  (try
    (zero? (:exit (sh/sh "sh" "-c" "command -v node")))
    (catch Throwable _ false)))

(deftest gen-handler-source--fingerprint-fields-emit-a-generated-fingerprint
  ;; The overrides stay external to the bundle, so an overrides import of the
  ;; runtime would load a second copy of its logging state.
  (let [source (g/gen-handler-source
                sample-fndefs
                (assoc mylib-overrides :fingerprint-fields [:dbBytes :iniBytes :logLevel]))]
    (is (str/includes? source "import { makeHandler, byteLengthFingerprint } from")
        "the helper is imported alongside makeHandler, from the same one copy")
    (is (str/includes? source "byteLengthFingerprint(\n  [\"dbBytes\", \"iniBytes\", \"logLevel\"],\n  null,\n)"))
    (is (str/includes? source "  fingerprint,\n"))
    (is (not (str/includes? source "overrides.fingerprint"))
        "the overrides export is no longer consulted")))

(defn- prefixed-source [prefix]
  (g/gen-handler-source sample-fndefs (assoc mylib-overrides
                                             :fingerprint-fields [:dbBytes]
                                             :fingerprint-prefix prefix)))

(deftest gen-handler-source--fingerprint-prefix-labels-the-handler
  (is (str/includes? (prefixed-source "gdal")
                     "byteLengthFingerprint(\n  [\"dbBytes\"],\n  \"gdal\",\n)"))
  (testing "the prefix is a JS string literal, so a quote in it cannot end it"
    (is (str/includes? (prefixed-source "o'b\"x") "  \"o'b\\\"x\",\n"))))

(deftest gen-handler-source--output-parses-under-node
  ;; The fingerprint variant emits every form, the trailing-comma call included.
  (let [source (g/gen-handler-source
                sample-fndefs
                (assoc mylib-overrides
                       :fingerprint-fields [:dbBytes]
                       :fingerprint-prefix "mylib"))]
    ;; Assert before the node check, so a runner with no node still asserts.
    (is (str/includes? source "export default create;"))
    (if-not (node-on-path?)
      (println "SKIP node --check: node not on PATH")
      (let [tmp (java.io.File/createTempFile "gen-handler-" ".mjs")]
        (try
          (spit tmp source)
          (let [{:keys [exit out err]} (sh/sh "node" "--check" (.getAbsolutePath tmp))]
            (is (zero? exit)
                (str "node --check failed (exit " exit ")\nstdout: " out
                     "\nstderr: " err "\n--- begin source ---\n" source
                     "\n--- end source ---")))
          (finally (.delete tmp)))))))

(deftest write-handler!--writes-file
  (let [tmp (java.io.File/createTempFile "gen-handler-write-" ".mjs")]
    (try
      (let [path (g/write-handler! {} mylib-overrides (.getAbsolutePath tmp))
            content (slurp path)]
        (is (= (.getAbsolutePath tmp) path))
        (is (str/includes? content "export default create"))
        (is (str/includes? content "import { makeHandler, byteLengthFingerprint }")))
      (finally (.delete tmp)))))
