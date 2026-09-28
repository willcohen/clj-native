;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.version-test
  "build.clj holds the one version of record. These tests pin its copies in
   deps.edn and package.json, and forbid it in bb.edn, README.md and
   .gitignore."
  (:require [clojure.test :refer [deftest is]]))

(defn- slurp-root [f]
  (let [file (java.io.File. ^String f)]
    (assert (.exists file) (str "expected to run from the repo root; no " f))
    (slurp file)))

(def ^:private canonical
  (second (re-find #"\(def version \"([^\"]+)\"\)" (slurp-root "build.clj"))))

(defn- npm-version
  "The top-level \"version\" of package.json. A regex reads it, since cheshire
   is not on the JVM test classpath and no other key is named \"version\"."
  []
  (second (re-find #"\"version\"\s*:\s*\"([^\"]+)\"" (slurp-root "package.json"))))

(deftest package-json-matches-the-canonical-version
  (is (= canonical (npm-version))))

(deftest deps-edn-deploy-artifact-points-at-the-built-jar
  (is (= [canonical] (mapv second (re-seq #"target/native-([0-9][^\s\"]*)\.jar"
                                          (slurp-root "deps.edn"))))))

(deftest no-other-file-names-the-version
  (doseq [f ["bb.edn" "README.md" ".gitignore"]]
    (is (not-any? #{canonical} (re-seq #"\b\d+\.\d+\.\d+\b" (slurp-root f)))
        (str f " names the version " canonical))))
