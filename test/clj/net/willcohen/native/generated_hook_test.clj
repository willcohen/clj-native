;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.generated-hook-test
  "Checks that the clj-kondo hook's copy of macros/camel-name->clj-name
   agrees with the original. The hook runs in clj-kondo's SCI, so it cannot
   require the project and carries its own copy."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [net.willcohen.native.macros :as m]))

(def ^:private hook-resource
  "clj-kondo.exports/net.willcohen/native/hooks/native/generated.clj")

(def ^:private hook-fn-names '#{upper-char? lower-char? camel-name->clj-name})

(def ^:private hook-ns 'net.willcohen.native.generated-hook-test.hook)

(defn- load-hook-fns!
  "Load the hook's pure string fns into hook-ns. The whole file cannot load on
   the JVM, since it requires clj-kondo.hooks-api."
  []
  (let [url (io/resource hook-resource)
        _ (assert url (str "hook not on the classpath: " hook-resource))
        forms (read-string (str "[" (slurp url) "]"))
        wanted (filter #(and (seq? %)
                             ('#{defn defn-} (first %))
                             (hook-fn-names (second %)))
                       forms)]
    (load-string (string/join "\n"
                              (cons (str "(ns " hook-ns " (:require [clojure.string :as string]))")
                                    (map pr-str wanted))))))

(def ^:private hook-loaded (delay (load-hook-fns!)))

;; A renamed or missing hook fn fails the load, or ns-resolve gives nil and the
;; call throws, so the parity test cannot pass with nothing to compare.
(defn- hook-camel-name->clj-name [s]
  @hook-loaded
  ((ns-resolve hook-ns 'camel-name->clj-name) s))

(def ^:private corpus
  "The camel names macros_test pins, plus edge shapes: single characters,
   underscores, digits and the empty string."
  ["GDALOpenEx" "CSLAddString" "GDALGetDriverCount" "VSIFree"
   "CPLHTTPSetFetchCallback" "GDALRasterIO" "OSRExportToPROJJSON"
   "OSRImportFromEPSG" "OGR_L_GetName" "OGR_F_GetFID" "OGR_Fld_GetNameRef"
   "OGR_F_GetFieldAsInteger64" "malloc"
   "A" "AB" "aB" "_" "__" "_Leading" "Trailing_" "A_B" "x1Y2" "HTTPSProxy"
   "proj_create_crs_to_crs" "GDAL2Tiles" "OGRGeometryH" "ABc" "aBC" ""])

(deftest hook-and-macros-camel-mappings-agree
  (doseq [s corpus]
    (is (= (str (m/camel-name->clj-name s)) (hook-camel-name->clj-name s))
        (str "divergent mapping for " (pr-str s)))))
