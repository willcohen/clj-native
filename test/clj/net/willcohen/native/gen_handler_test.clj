;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.gen-handler-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.java.io :as io]
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

(deftest gen-handler-source--module-parts
  (let [source (g/gen-handler-source sample-fndefs mylib-overrides)]
    ;; A module worker ignores the page importmap, so a static bare import
    ;; of ffi-wasm fails in a browser worker.
    (testing "imports"
      (is (not (re-find #"(?m)^import .* from 'ffi-wasm" source)))
      (is (str/includes? source "import * as overrides from './mylib-handler-overrides.mjs'"))
      (is (str/includes? source "await import(initArgs?.ffiWasmHandlerUrl ?? 'ffi-wasm/handler')")))
    (testing "classification arrays"
      (is (str/includes? source (str "const busyMethods = [\"ccall\", \"malloc\", \"context_create\", "
                                     "\"set_log_level\", \"heapf64_set\", \"heapf64_get\", "
                                     "\"read_string_array\", \"heapu8_set\", \"heapu8_get\", "
                                     "\"string_to_utf8\", \"utf8_to_string\"];\n")))
      (is (str/includes? source
                         "const destroyMethods = [\"context_destroy\", \"free\", \"shutdown\"];\n")))
    (testing "one methods entry per exposed method, from overrides.methods(ffi)"
      (is (str/includes? source "const impl = overrides.methods(ffi);"))
      (is (str/includes? source "const methods = {"))
      (doseq [m [:context_create :ccall :malloc :free :shutdown :heapf64_set]]
        (is (str/includes? source (str (name m) ": impl." (name m) ","))
            (str "method " (name m) " missing from emitted methods object"))))
    ;; worker-router's worker-bootstrap runs teardown only when
    ;; `typeof mod.destroy === 'function'`.
    (testing "the destroy export"
      (is (str/includes? source "export const destroy = overrides.destroy;")))))

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

(deftest gen-handler-source--required-keys
  (doseq [k [:overrides-import-path :exposed-methods :fingerprint-fields]]
    (is (thrown-with-msg? Exception (re-pattern (str "missing required override key " k))
                          (g/gen-handler-source sample-fndefs (dissoc mylib-overrides k))))))

(deftest gen-handler-source--classification-is-optional
  ;; The runtime queue is serial, so the classification affects trace fields only.
  (let [source (g/gen-handler-source
                sample-fndefs
                (dissoc mylib-overrides :busy-methods :destroy-methods))]
    (is (str/includes? source "const busyMethods = [];"))
    (is (str/includes? source "const destroyMethods = [];"))))

(deftest gen-handler-source--fingerprint-fields-emit-a-generated-fingerprint
  (let [source (g/gen-handler-source
                sample-fndefs
                (assoc mylib-overrides :fingerprint-fields [:dbBytes :iniBytes :logLevel]))]
    (is (str/includes? source "const fingerprintFields = [\"dbBytes\", \"iniBytes\", \"logLevel\"];\n"))
    (is (str/includes? source "const fingerprintPrefix = null;\n"))
    (is (str/includes? source "fingerprint: ffi.byteLengthFingerprint(fingerprintFields, fingerprintPrefix),")
        "the helper comes from the same ffi-wasm copy as makeHandler")
    (is (not (str/includes? source "overrides.fingerprint"))
        "the overrides export is no longer consulted")))

(defn- prefixed-source [prefix]
  (g/gen-handler-source sample-fndefs (assoc mylib-overrides
                                             :fingerprint-fields [:dbBytes]
                                             :fingerprint-prefix prefix)))

(deftest gen-handler-source--fingerprint-prefix-labels-the-handler
  (is (str/includes? (prefixed-source "gdal") "const fingerprintPrefix = \"gdal\";\n"))
  (testing "the prefix is a JS string literal, so a quote in it cannot end it"
    (is (str/includes? (prefixed-source "o'b\"x") "const fingerprintPrefix = \"o'b\\\"x\";\n"))))

(deftest gen-handler-source--label-goes-to-makeHandler
  (is (str/includes? (g/gen-handler-source sample-fndefs mylib-overrides) "  label: null,\n"))
  (is (str/includes? (g/gen-handler-source sample-fndefs (assoc mylib-overrides :label "cg.wasmts"))
                     "  label: \"cg.wasmts\",\n")))

(deftest gen-handler-source--output-parses-under-node
  ;; With a prefix, the output holds every form the generator emits.
  (let [source (g/gen-handler-source
                sample-fndefs
                (assoc mylib-overrides :fingerprint-prefix "mylib"))
        tmp    (java.io.File/createTempFile "gen-handler-" ".mjs")]
    (is (str/includes? source "export default create;"))
    (try
      (spit tmp source)
      (let [{:keys [exit out err]} (sh/sh "node" "--check" (.getAbsolutePath tmp))]
        (is (zero? exit)
            (str "node --check failed (exit " exit ")\nstdout: " out
                 "\nstderr: " err "\n--- begin source ---\n" source
                 "\n--- end source ---")))
      (finally (.delete tmp)))))

(deftest write-handler!--writes-file
  (let [tmp (java.io.File/createTempFile "gen-handler-write-" ".mjs")]
    (try
      (let [path (g/write-handler! sample-fndefs mylib-overrides (.getAbsolutePath tmp))]
        (is (= (.getAbsolutePath tmp) path))
        (is (= (g/gen-handler-source sample-fndefs mylib-overrides) (slurp path))))
      (finally (.delete tmp)))))

(def ^:private run-overrides
  {:overrides-import-path "./overrides.mjs"
   :exposed-methods [:probe]
   :busy-methods [:probe]
   :fingerprint-fields [:dbBytes]
   :label "fixture"})

;; With a URL argument, the first create gets a URL that does not load, and
;; the second gets the good one.
(def ^:private run-driver
  "import { create, destroy } from './handler.mjs';
const good = process.argv[2];
const log = { handlerRuntime: { logLevel: 'debug', logCategories: ['busy'] } };
const out = {};
if (good) {
  out.bad = await create({ ffiWasmHandlerUrl: 'file:///nonexistent/ffi-wasm-handler.mjs', dbBytes: new Uint8Array(1) })
    .then(() => 'resolved', () => 'rejected');
}
const h = await create({ ...(good ? { ffiWasmHandlerUrl: good } : {}), dbBytes: new Uint8Array(1), tag: 't', ...log });
out.probe = await h.probe();
out.destroy = await destroy();
console.log(JSON.stringify(out));
")

(defn- run-generated-handler
  "Write a generated handler, the fixture overrides and a driver into a new
   dir under target/, and run the driver with node. The dir is inside the
   repo, so the bare specifier ffi-wasm/handler resolves to this package."
  [& driver-args]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      (.toPath (doto (io/file "target") .mkdirs))
                      "gen-handler-run-"
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (io/copy (io/file "test/fixtures/gen-handler-overrides.mjs") (io/file dir "overrides.mjs"))
      (g/write-handler! {} run-overrides (io/file dir "handler.mjs"))
      (spit (io/file dir "driver.mjs") run-driver)
      (let [{:keys [exit out err]} (apply sh/sh "node" "driver.mjs" (concat driver-args [:dir dir]))]
        {:exit exit :out out :err err
         :result (last (str/split-lines out))})
      (finally
        (doseq [f (reverse (file-seq dir))] (.delete f))))))

(def ^:private ffi-wasm-handler-url
  (str (.toUri (.toPath (io/file "src/cljc/net/willcohen/native/handler.mjs")))))

(def ^:private expected-probe
  (str "\"probe\":{\"sameFfi\":true,\"makeHandler\":\"function\",\"stageFiles\":\"function\","
       "\"createSyncFetch\":\"function\",\"tag\":\"t\"}"))

(deftest generated-handler--loads-ffi-wasm-from-the-init-url
  (let [{:keys [exit out err result]} (run-generated-handler ffi-wasm-handler-url)]
    (is (zero? exit) (str "node failed\nstdout: " out "\nstderr: " err))
    (is (= (str "{\"bad\":\"rejected\"," expected-probe ",\"destroy\":\"destroyed\"}") result)
        (str "a URL that does not load rejects create, a later create retries, "
             "and init and methods get one ffi-wasm namespace"))
    (is (str/includes? out "BUSY-INC] fn=probe label=fixture")
        "the :label reaches the trace events of makeHandler")))

(deftest generated-handler--falls-back-to-the-bare-specifier-with-no-url
  (let [{:keys [exit out err result]} (run-generated-handler)]
    (is (zero? exit) (str "node failed\nstdout: " out "\nstderr: " err))
    (is (= (str "{" expected-probe ",\"destroy\":\"destroyed\"}") result))))
