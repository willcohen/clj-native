;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.version-test
  "build.clj holds the one version of record. These tests pin its copies in
   deps.edn and package.json, and forbid version literals in bb.edn,
   README.md and .gitignore."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(defn- slurp-root [f]
  (let [file (java.io.File. ^String f)]
    (assert (.exists file) (str "expected to run from the repo root; no " f))
    (slurp file)))

(def ^:private canonical
  (second (re-find #"\(def version \"([^\"]+)\"\)" (slurp-root "build.clj"))))

(defn- captures [re s]
  (mapv second (re-seq re s)))

(defn- npm-version
  "The top-level \"version\" of package.json. A regex reads it, since cheshire
   is not on the JVM test classpath and no other key is named \"version\"."
  []
  (second (re-find #"\"version\"\s*:\s*\"([^\"]+)\"" (slurp-root "package.json"))))

(deftest build-clj-declares-a-canonical-version
  (is (some? canonical) "build.clj must carry (def version \"...\")")
  (is (re-matches #"\d+\.\d+\.\d+.*" canonical)))

(deftest package-json-matches-the-canonical-version
  (is (some? (npm-version)) "package.json must carry a top-level \"version\"")
  (is (= canonical (npm-version))))

(deftest deps-edn-deploy-artifact-points-at-the-built-jar
  (let [found (captures #"native-([0-9][^\s\"]*)\.jar" (slurp-root "deps.edn"))]
    (testing "the jar path was located, so the check is not vacuous"
      (is (= 1 (count found)) (str "found: " (pr-str found))))
    (is (= [canonical] found) (str "drifted: " (pr-str found)))
    (is (str/includes? (slurp-root "deps.edn")
                       (str "target/native-" canonical ".jar"))
        "the :deploy :artifact path must name the jar build.clj produces")))

(deftest no-other-file-names-the-version
  (doseq [f ["bb.edn" "README.md" ".gitignore"]]
    (is (not-any? #{canonical} (re-seq #"\b\d+\.\d+\.\d+\b" (slurp-root f)))
        (str f " names the version " canonical))))
