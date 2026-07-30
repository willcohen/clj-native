;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.containerfile-test
  "Pins the Containerfile that ships inside the jar.

   Nothing else exercises it. build.clj's containerfile-path! reads it off the
   classpath at net/willcohen/native/Containerfile, falling back to a
   consumer's own copy in the working directory, and run-in-container! names a
   build stage as its --target. Both couplings are by string, so a moved
   resource or a renamed stage stays quiet until a consumer's container build
   fails.

   The swallow check exists because a RUN ending in `|| echo` exits 0 whatever
   the command inside it did. The test-playwright stage carried one, so a
   failing browser suite printed a reassuring line and the image still built
   green. A `||` handing off to a real recovery command (the git bootstrap at
   the top) is a different thing and stays allowed."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private resource-path "net/willcohen/native/Containerfile")

(defn- containerfile [] (io/resource resource-path))

(deftest containerfile-ships-where-build-clj-looks-for-it
  (is (some? (containerfile))
      "extract-resource! and repo-root-from-resource read exactly this path"))

(deftest no-stage-swallows-the-failure-of-what-it-ran
  (let [swallowing (->> (str/split-lines (slurp (containerfile)))
                        (filter #(re-find #"\|\|\s*(echo|true|:)(\s|$)" %)))]
    (is (empty? swallowing)
        (str "a command ending in `|| echo` (or `|| true`) exits 0 whatever it "
             "ran, so its stage cannot fail: " (pr-str swallowing)))))

(deftest every-documented-target-stage-exists
  (let [source (slurp (containerfile))
        stage? (fn [s] (re-find (re-pattern (str "(?m)^FROM .* AS " s "\\s*$")) source))]
    (testing "build.clj's run-in-container! defaults :target to native-build"
      (is (stage? "native-build")))
    (testing "the stages this file's own usage block tells a consumer to build"
      (doseq [s ["wasm-build" "test-all" "dev" "export"]]
        (is (stage? s) (str "missing stage: " s))))))
