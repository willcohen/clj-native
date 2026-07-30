;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.handler-paths-test
  "Coverage for handler-paths against the real filesystem.

   resolveAsset exists because the same probe is off-by-one prone in every
   consumer, so the tests use real directories rather than a stubbed existsSync:
   a wrong number of levels has to actually fail. The suite writes its own
   temporary tree, and reads test/fixtures through a relative candidate list
   resolved from this compiled test file's own location."
  (:require [cljs.test :refer [deftest is testing]]
            ["ffi-wasm/handler-paths" :refer [resolveAsset loadEmscriptenModule]]
            ["ffi-wasm/test-runner" :as tr]
            ["node:fs" :refer [mkdirSync mkdtempSync writeFileSync]]
            ["node:os" :refer [tmpdir]]
            ["node:path" :refer [join]]))

(def ^:private here (.-url js/import.meta))

;; This file compiles to test/cljs/net/willcohen/native/, so test/fixtures is
;; four levels up. A shorter or longer climb is exactly the mistake resolveAsset
;; is meant to make loud, so the count is spelled out rather than derived.
(def ^:private fixtures-candidates #js [#js [".." ".." ".." ".." "fixtures"]])

(defn- temp-tree!
  "Create <tmp>/a and <tmp>/b, put `name` in whichever of them `in` names, and
   return the two absolute dirs. Left behind for the OS to reap; a test that
   removed it would race the next one's probe."
  [name in]
  (let [root (mkdtempSync (join (tmpdir) "handler-paths-test-"))
        a (join root "a")
        b (join root "b")]
    (mkdirSync a)
    (mkdirSync b)
    (writeFileSync (join (if (= in :a) a b) name) "x")
    {:root root :a a :b b}))

(deftest ^:async resolve-asset-returns-the-first-candidate-holding-the-name
  (let [{:keys [a b]} (temp-tree! "found.dat" :a)
        r (await (resolveAsset here "found.dat" #js [a b]))]
    (is (= a (.-dir r)) "the first hit wins")
    (is (= (join a "found.dat") (.-path r))
        "path is the dir joined with the name, ready to open")))

(deftest ^:async resolve-asset-falls-through-a-miss-to-a-later-candidate
  (let [{:keys [a b]} (temp-tree! "later.dat" :b)
        r (await (resolveAsset here "later.dat" #js [a b]))]
    (is (= b (.-dir r)) "an empty earlier candidate is skipped, not fatal")))

(deftest ^:async resolve-asset-accepts-segment-arrays-and-relative-strings
  (testing "an array of segments resolves against the caller's own directory"
    (let [r (await (resolveAsset here "fake-emscripten.mjs" fixtures-candidates))]
      (is (.endsWith (.-dir r) (join "test" "fixtures")))))
  (testing "a relative string resolves the same way"
    (let [r (await (resolveAsset here "fake-emscripten.mjs"
                                 #js [(join ".." ".." ".." ".." "fixtures")]))]
      (is (.endsWith (.-dir r) (join "test" "fixtures"))))))

(deftest ^:async resolve-asset-lists-every-probed-path-when-it-misses
  (let [{:keys [a b]} (temp-tree! "elsewhere.dat" :a)]
    (try
      (await (resolveAsset here "absent.dat" #js [a b]))
      (is false "resolveAsset should have thrown")
      (catch :default e
        (let [msg (.-message e)]
          (is (.includes msg "absent.dat"))
          (testing "both probed dirs are named, so the caller can see the miss"
            (is (.includes msg a))
            (is (.includes msg b))))))))

(deftest ^:async resolve-asset-guards-its-arguments
  (testing "the name must be a non-empty string"
    (is (thrown-with-msg? js/Error #"name must be"
                          (await (resolveAsset here "" #js [#js ["."]]))))
    (is (thrown-with-msg? js/Error #"name must be"
                          (await (resolveAsset here nil #js [#js ["."]])))))
  (testing "the candidate list must be a non-empty array"
    (is (thrown-with-msg? js/Error #"candidates must be"
                          (await (resolveAsset here "x.dat" #js []))))
    (is (thrown-with-msg? js/Error #"candidates must be"
                          (await (resolveAsset here "x.dat" nil)))))
  (testing "a candidate that is neither a string nor an array is rejected by type"
    (is (thrown-with-msg? js/Error #"candidate must be a string or array"
                          (await (resolveAsset here "x.dat" #js [42]))))))

(deftest ^:async load-emscripten-module-returns-the-default-export-and-a-locate-file
  (let [r (await (loadEmscriptenModule here #js {:name "fake-emscripten.mjs"
                                                 :candidates fixtures-candidates}))]
    (is (fn? (.-factory r)) "factory is the module's default export")
    (let [m (await ((.-factory r) #js {}))]
      (is (= "fake-emscripten" (.-marker m)) "invoking it produces the module"))
    (testing "locateFile points a sibling name at the directory the import came from"
      (is (= (join (.-dir r) "fake-emscripten.wasm")
             ((.-locateFile r) "fake-emscripten.wasm"))))))

(deftest ^:async load-emscripten-module-takes-a-node-only-name-override
  (let [r (await (loadEmscriptenModule here #js {:nodeName "fake-emscripten.mjs"
                                                 :browserName "never-loaded-here.mjs"
                                                 :candidates fixtures-candidates}))]
    (is (fn? (.-factory r)) "the Node name is the one used on Node")))

(deftest ^:async load-emscripten-module-needs-a-name
  (is (thrown-with-msg? js/Error #"needs a `name`"
                        (await (loadEmscriptenModule here #js {:candidates fixtures-candidates}))))
  (is (thrown-with-msg? js/Error #"needs a `name`"
                        (await (loadEmscriptenModule here nil)))))

(tr/run-tests-and-exit! "net.willcohen.native.handler-paths-test")
