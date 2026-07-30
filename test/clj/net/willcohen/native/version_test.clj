;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.version-test
  "build.clj holds the one version of record. deps.edn and package.json
   cannot read it, so they are pinned here. Every other file must carry no
   version literal. Each pattern asserts its match count, so a rename cannot
   make a check vacuous."
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
  "The top-level \"version\" of package.json, read with a regex rather than a
   JSON parser. cheshire ships with babashka but is not on the JVM test
   classpath, and no other key in the file is named \"version\"."
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

(deftest bb-edn-carries-no-version-literal
  (testing "bb.edn reads the version from build.clj at :init, so a hardcoded
            one is drift waiting to happen"
    (let [bb (slurp-root "bb.edn")
          tokens (re-seq #"\b\d+\.\d+\.\d+\b" bb)
          project-tokens (filter #(= canonical %) tokens)]
      (is (empty? project-tokens)
          (str "bb.edn must name no project version; found "
               (count project-tokens)
               ". Use the project-version binding from :init instead."))))
  (testing "the :init binding still resolves, so the tasks print a real number"
    (let [bb (slurp-root "bb.edn")]
      (is (str/includes? bb "(def project-version")
          "bb.edn :init must define project-version")
      (is (str/includes? bb "project-version \".jar\"")
          "the jar task must compose its path from project-version")))
  ;; some?, not (= canonical ...): canonical comes from the same pattern, so an
  ;; equality check here agrees with itself even when both read nil.
  (testing "the pattern bb.edn's :init carries still matches build.clj"
    (let [bb-pattern (re-pattern "\\(def version \"([^\"]+)\"\\)")]
      (is (some? (second (re-find bb-pattern (slurp-root "build.clj"))))
          "bb.edn's :init regex must still find build.clj's (def version ...)"))))

(deftest readme-carries-no-version-literal
  (testing "the README names no version, so it has nothing to drift"
    (let [tokens (re-seq #"\b\d+\.\d+\.\d+\b" (slurp-root "README.md"))]
      (is (empty? tokens)
          (str "README must name no version; found " (pr-str tokens)
               ". If a version site returns on purpose, pin it here instead.")))))

(deftest gitignore-carries-no-version-literal
  (testing "the tarball ignore is a glob, and its comment must not pin a version"
    (let [tokens (re-seq #"\b\d+\.\d+\.\d+\b" (slurp-root ".gitignore"))]
      (is (empty? tokens)
          (str ".gitignore must name no version; found " (pr-str tokens))))))

(deftest the-npm-and-clojars-versions-are-the-same-release
  (testing "the two ecosystems ship one version number, not two"
    (is (= (npm-version) canonical)
        (str "npm and Clojars must agree; canonical is " canonical))))
