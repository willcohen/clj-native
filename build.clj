;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns build
  "tools.build tasks for the jar of this project. The consumer build API is
   net.willcohen.native.build."
  (:require [clojure.tools.build.api :as b]))

(def lib 'net.willcohen/native)
(def version "0.0.1")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

;; delay: resolve artifacts only when a task runs.
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn pom [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                ;; The pom takes one dir and warns about the rest. copy-dir
                ;; fills the jar.
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

;; The private tools.build default-ignores plus every .mjs. The squint output
;; is gitignored, so the jar would hold whatever build:js last left, and no
;; JVM consumer loads a .mjs. npm ships the JS.
(def ^:private jar-ignores [".*~$" "^#.*#$" "^\\.#.*" "^.DS_Store$" ".*\\.mjs$"])

(defn jar [_]
  (clean nil)
  (pom nil)
  (b/copy-dir {:src-dirs ["src/clj" "src/cljc" "src/bb" "resources"]
               :target-dir class-dir
               :ignores jar-ignores})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))
