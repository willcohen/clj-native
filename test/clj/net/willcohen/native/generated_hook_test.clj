;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.generated-hook-test
  "Parity coverage for the shipped clj-kondo hook's copy of the camel-case
   name mapping.

   The hook runs inside clj-kondo's SCI and cannot call into the project it
   lints, so hooks/native/generated.clj carries its own copy of
   macros/camel-name->clj-name. That mapping decides the public name of every
   generated fn in every consumer, and two hand-maintained copies of it drift.

   The suite lifts the mapping out of the hook file and loads it on the JVM,
   which the hook's own require of clj-kondo.hooks-api would otherwise
   prevent. Only the three pure string fns are lifted; the node builders need
   the hooks API and stay untested here."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [net.willcohen.native.macros :as m]))

(def ^:private hook-resource
  "clj-kondo.exports/net.willcohen/native/hooks/native/generated.clj")

(def ^:private lifted-fn-names '#{upper-char? lower-char? camel-name->clj-name})

(defn- lift-hook-fns!
  "Read the hook file, keep the pure string fns, and load them into `ns-sym`.
   Returns the number of fns lifted, so a rename in the hook shows up as a
   count mismatch rather than as a suite that quietly tests nothing."
  [ns-sym]
  (let [url (io/resource hook-resource)
        _ (assert url (str "hook not on the classpath: " hook-resource))
        forms (read-string (str "[" (slurp url) "]"))
        wanted (filter #(and (seq? %)
                             ('#{defn defn-} (first %))
                             (lifted-fn-names (second %)))
                       forms)]
    (load-string (string/join "\n"
                              (cons (str "(ns " ns-sym " (:require [clojure.string :as string]))")
                                    (map pr-str wanted))))
    (count wanted)))

(def ^:private lifted-count (delay (lift-hook-fns! 'net.willcohen.native.generated-hook-test.lifted)))

(defn- hook-camel-name->clj-name [s]
  @lifted-count
  ((resolve 'net.willcohen.native.generated-hook-test.lifted/camel-name->clj-name) s))

(def ^:private corpus
  "Every camel name macros_test pins, plus shapes that suite never exercises:
   single characters, bare and doubled underscores, leading and trailing
   underscores, embedded digits, and the empty string."
  ["GDALOpenEx" "CSLAddString" "GDALGetDriverCount" "VSIFree"
   "CPLHTTPSetFetchCallback" "GDALRasterIO" "OSRExportToPROJJSON"
   "OSRImportFromEPSG" "OGR_L_GetName" "OGR_F_GetFID" "OGR_Fld_GetNameRef"
   "OGR_F_GetFieldAsInteger64" "malloc"
   "A" "AB" "aB" "_" "__" "_Leading" "Trailing_" "A_B" "x1Y2" "HTTPSProxy"
   "proj_create_crs_to_crs" "GDAL2Tiles" "OGRGeometryH" "ABc" "aBC" ""])

(deftest the-hook-still-carries-the-three-string-fns
  (is (= (count lifted-fn-names) @lifted-count)
      "a rename in the hook would leave the parity test below asserting nothing"))

(deftest hook-and-macros-camel-mappings-agree
  (testing "both copies produce the same public name for every corpus entry"
    (doseq [s corpus]
      (is (= (str (m/camel-name->clj-name s)) (hook-camel-name->clj-name s))
          (str "divergent mapping for " (pr-str s)))))
  (testing "the hook returns a string where macros returns a symbol"
    (is (string? (hook-camel-name->clj-name "GDALOpenEx")))
    (is (symbol? (m/camel-name->clj-name "GDALOpenEx")))))
