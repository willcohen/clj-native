;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.macros-test
  "Dual-runtime coverage for the surface-generation helpers.

   These helpers decide the public name of every generated fn, so a
   difference between the two lanes would rename part of a consumer's
   API on one platform only. The suite therefore runs the same bodies
   under JVM clojure.test and squint cljs.test.

   Every input here is a plain string, because `name` accepts a string
   on both platforms while squint has no keyword reader. The JVM-only
   intern loop sits behind a reader conditional."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj  [net.willcohen.native.macros :as m]
               :cljs ["ffi-wasm/macros" :as m])
            #?(:cljs ["ffi-wasm/test-runner" :as tr])))

(deftest c-name->clj-name-hyphenates-underscores
  (is (= "proj-create-crs-to-crs" (str (m/c-name->clj-name "proj_create_crs_to_crs"))))
  (is (= "proj-destroy" (str (m/c-name->clj-name "proj_destroy"))))
  (is (= "nounderscores" (str (m/c-name->clj-name "nounderscores")))))

(deftest camel-name->clj-name-splits-words-and-keeps-acronyms
  (testing "a capital that follows a lower-case letter opens a word"
    (is (= "gdal-open-ex" (str (m/camel-name->clj-name "GDALOpenEx"))))
    (is (= "csl-add-string" (str (m/camel-name->clj-name "CSLAddString")))))
  (testing "a leading upper-case run stays in one piece"
    (is (= "gdal-get-driver-count" (str (m/camel-name->clj-name "GDALGetDriverCount"))))
    (is (= "vsi-free" (str (m/camel-name->clj-name "VSIFree"))))
    (is (= "cplhttp-set-fetch-callback"
           (str (m/camel-name->clj-name "CPLHTTPSetFetchCallback")))))
  (testing "a trailing upper-case run stays in one piece"
    (is (= "gdal-raster-io" (str (m/camel-name->clj-name "GDALRasterIO"))))
    (is (= "osr-export-to-projjson" (str (m/camel-name->clj-name "OSRExportToPROJJSON"))))
    (is (= "osr-import-from-epsg" (str (m/camel-name->clj-name "OSRImportFromEPSG")))))
  (testing "underscores become single separators"
    (is (= "ogr-l-get-name" (str (m/camel-name->clj-name "OGR_L_GetName"))))
    (is (= "ogr-f-get-fid" (str (m/camel-name->clj-name "OGR_F_GetFID"))))
    (is (= "ogr-fld-get-name-ref" (str (m/camel-name->clj-name "OGR_Fld_GetNameRef")))))
  (testing "digits stay attached to the word they end"
    (is (= "ogr-f-get-field-as-integer64"
           (str (m/camel-name->clj-name "OGR_F_GetFieldAsInteger64")))))
  (testing "an all-lower-case name is unchanged"
    (is (= "malloc" (str (m/camel-name->clj-name "malloc"))))))

(deftest fn-def-arg-syms-follows-the-c-signature
  (is (= ["dataset" "band-num"]
         (mapv str (m/fn-def-arg-syms {:argtypes [["dataset" "pointer"]
                                                  ["band-num" "int32"]]}))))
  (is (= [] (mapv str (m/fn-def-arg-syms {:argtypes []}))))
  (testing "a repeated name is suffixed, so the second does not shadow the first"
    (is (= ["ptr" "ptr-2" "ptr-3"]
           (mapv str (m/fn-def-arg-syms {:argtypes [["ptr" "pointer"]
                                                    ["ptr" "pointer"]
                                                    ["ptr" "pointer"]]}))))))

(def ^:private two-fndefs
  {"AlphaOne" {:argtypes []}
   "BetaTwo"  {:argtypes [["x" "int32"]]}})

(deftest library-fns-form-walks-fndefs-once-per-emitter
  (let [form (m/library-fns-form two-fndefs
                                 {:name-fn m/camel-name->clj-name
                                  :emit-fn (fn [fn-name fn-key _] (str fn-name "<-" fn-key))})]
    (is (= "do" (str (first form))))
    (is (= #{"alpha-one<-AlphaOne" "beta-two<-BetaTwo"} (set (rest form))))))

(deftest library-fns-form-skips-a-key-the-name-fn-declines
  (let [form (m/library-fns-form two-fndefs
                                 {:name-fn (fn [fn-key]
                                             (when (= fn-key "BetaTwo")
                                               (m/camel-name->clj-name fn-key)))
                                  :emit-fn (fn [fn-name _ _] (str fn-name))})]
    (is (= ["beta-two"] (vec (rest form)))
        "a nil name leaves the entry out of the surface entirely")))

(deftest library-fns-form-appends-the-alias-walk
  (let [form (m/library-fns-form two-fndefs
                                 {:name-fn m/camel-name->clj-name
                                  :emit-fn (fn [fn-name _ _] (str fn-name))
                                  :alias-name-fn (fn [fn-key]
                                                   (when (= fn-key "AlphaOne") "a1"))
                                  :alias-emit-fn (fn [fn-name _ _] (str "alias:" fn-name))})
        parts (vec (rest form))]
    (is (= 3 (count parts)) "two primary fns plus one alias")
    (is (= "alias:a1" (last parts)) "the alias walk runs after the primary walk")))

(deftest library-fns-form-defaults-to-the-underscore-mapping
  (let [form (m/library-fns-form {"a_b" {:argtypes []}}
                                 {:emit-fn (fn [fn-name _ _] (str fn-name))})]
    (is (= ["a-b"] (vec (rest form))))))

#?(:clj
   (deftest intern-library-fns!-interns-one-var-per-entry
     (let [target (create-ns 'net.willcohen.native.macros-test.target)]
       (try
         (m/intern-library-fns! (ns-name target) two-fndefs
                                m/camel-name->clj-name
                                (fn [fn-key _fn-def] (fn [] fn-key)))
         (is (= "AlphaOne" ((ns-resolve target 'alpha-one))))
         (is (= "BetaTwo" ((ns-resolve target 'beta-two))))
         (finally (remove-ns (ns-name target)))))))

#?(:clj
   (deftest intern-library-fns!-skips-a-key-the-name-fn-declines
     (let [target (create-ns 'net.willcohen.native.macros-test.partial-target)]
       (try
         (m/intern-library-fns! (ns-name target) two-fndefs
                                (fn [fn-key] (when (= fn-key "BetaTwo") 'beta-two))
                                (fn [_ _] (fn [] :ok)))
         (is (nil? (ns-resolve target 'alpha-one)) "declined key interns nothing")
         (is (some? (ns-resolve target 'beta-two)))
         (finally (remove-ns (ns-name target)))))))

#?(:cljs (tr/run-tests-and-exit! "net.willcohen.native.macros-test"))
