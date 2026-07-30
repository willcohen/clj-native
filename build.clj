;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns build
  "tools.build for the jar of this project. This is NOT the consumer-facing
   build API, which is net.willcohen.native.build under src/bb."
  (:require [clojure.tools.build.api :as b]))

(def lib 'net.willcohen/native)
(def version "0.0.1")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

;; delay to defer artifact resolution until a task actually runs
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn pom [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                ;; write-pom fills the pom's single <sourceDirectory> from the
                ;; first entry and prints "Skipping paths:" for the rest.
                ;; Passing only src/clj says the same thing without the noise;
                ;; jar content comes from copy-dir below, not from this key.
                :src-dirs ["src/clj"]
                :pom-data [[:licenses
                            [:license
                             [:name "Apache-2.0 WITH LLVM-exception"]
                             [:url "https://llvm.org/LICENSE.txt"]
                             [:distribution "repo"]]]
                           [:description "Helper utilities for using native libraries and FFI in the Clojure and Squint (ClojureScript) ecosystems."]
                           [:developers
                            [:developer
                             [:name "Will Cohen"]]]
                           [:scm
                            [:url "https://github.com/willcohen/clj-native"]]]}))

;; tools.build's own defaults (clojure.tools.build.tasks.copy/default-ignores,
;; which is private) plus every .mjs. Six of the thirteen .mjs under src/cljc
;; are gitignored squint output, so including them would make jar content
;; depend on whatever `bb build:js` last left on disk. The other seven are
;; hand-written but equally inert on the JVM: no consumer loads a clj-native
;; .mjs off the classpath, and npm is the delivery path for the JS surface.
(def ^:private jar-ignores [".*~$" "^#.*#$" "^\\.#.*" "^.DS_Store$" ".*\\.mjs$"])

(defn jar [_]
  (clean nil)
  (pom nil)
  (b/copy-dir {:src-dirs ["src/clj" "src/cljc" "src/bb" "resources"]
               :target-dir class-dir
               :ignores jar-ignores})
  ;; The flake rides along as a resource so a consumer of the published jar can
  ;; vendor a flake input pinned to this exact version into a container build.
  ;; It cannot live under resources/ instead: nix requires a flake at the root
  ;; of its tree, and this repo's own flake is that root.
  (doseq [f ["flake.nix" "flake.lock"]]
    (b/copy-file {:src f
                  :target (format "%s/net/willcohen/native/%s" class-dir f)}))
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))
