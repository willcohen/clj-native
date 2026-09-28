;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.handler-fs-test
  "Tests of handler-fs's MEMFS staging against a stand-in module.FS that
   records each call."
  (:require [cljs.test :refer [deftest is testing]]
            ["ffi-wasm/handler-fs" :refer [stageFiles]]
            ["ffi-wasm/test-runner" :as tr]))

(defn- fake-fs
  "A stand-in module whose FS records each mkdirTree and writeFile."
  []
  (let [writes (atom [])
        trees (atom [])]
    {:module #js {:FS #js {:writeFile (fn [path data] (swap! writes conj [path data]) nil)
                           :mkdirTree (fn [dir] (swap! trees conj dir) nil)}}
     :writes writes
     :trees trees}))

(deftest stage-files-writes-each-file-and-returns-its-memfs-path
  (let [{:keys [module writes trees]} (fake-fs)
        out (stageFiles module
                        #js {"proj.db" (js/Uint8Array. #js [1 2 3])
                             "proj.ini" (js/Uint8Array. #js [4])}
                        "/proj")]
    (is (= ["/proj"] @trees))
    (is (= 2 (count @writes)))
    (testing "the returned map keys each basename to its absolute MEMFS path"
      (is (= "/proj/proj.db" (aget out "proj.db")))
      (is (= "/proj/proj.ini" (aget out "proj.ini"))))
    (testing "the bytes reach writeFile unchanged"
      (let [by-path (into {} (map (fn [[p d]] [p (vec (js/Array.from d))])) @writes)]
        (is (= [1 2 3] (get by-path "/proj/proj.db")))
        (is (= [4] (get by-path "/proj/proj.ini")))))))

(deftest stage-files-trims-one-trailing-slash-from-the-dir
  (let [{:keys [module]} (fake-fs)
        out (stageFiles module #js {"a.dat" (js/Uint8Array. #js [1])} "/data/")]
    (is (= "/data/a.dat" (aget out "a.dat"))
        "no doubled separator")))

(deftest stage-files-coerces-what-it-is-given-to-uint8
  (let [{:keys [module writes]} (fake-fs)
        backing (js/Uint8Array. #js [9 8 7 6])
        view (js/Uint8Array. (.-buffer backing) 1 2)]
    (stageFiles module
                #js {"u8.dat" (js/Uint8Array. #js [1 2])
                     "view.dat" view
                     "arr.dat" #js [5 6 7]
                     "ab.dat" (.-buffer (js/Uint8Array. #js [3 4]))}
                "/d")
    (let [by-path (into {} (map (fn [[p d]] [p d])) @writes)]
      (testing "every value reaches writeFile as a Uint8Array"
        (is (every? #(instance? js/Uint8Array %) (vals by-path))))
      (testing "a typed-array view keeps its own window, not the whole buffer"
        (is (= [8 7] (vec (js/Array.from (get by-path "/d/view.dat"))))))
      (testing "a plain array is copied element-wise"
        (is (= [5 6 7] (vec (js/Array.from (get by-path "/d/arr.dat"))))))
      (testing "a bare ArrayBuffer is bytes too, though it has no .buffer"
        ;; fetch().arrayBuffer() returns one.
        (is (= [3 4] (vec (js/Array.from (get by-path "/d/ab.dat")))))))))

(deftest stage-files-rejects-a-value-that-is-not-bytes
  (let [{:keys [module]} (fake-fs)]
    (is (thrown-with-msg? js/Error #"bad\.dat is not Uint8Array or coercible"
                          (stageFiles module #js {"bad.dat" 42} "/d"))
        "the message names the failing file")))

(deftest stage-files-guards-its-arguments
  (let [{:keys [module]} (fake-fs)
        bytes #js {"a.dat" (js/Uint8Array. #js [1])}]
    (testing "a module with no FS cannot stage anything"
      (is (thrown-with-msg? js/Error #"module\.FS not available"
                            (stageFiles #js {} bytes "/d")))
      (is (thrown-with-msg? js/Error #"module\.FS not available"
                            (stageFiles nil bytes "/d"))))
    (testing "the dir must be a non-empty string"
      (is (thrown-with-msg? js/Error #"memfsDir" (stageFiles module bytes "")))
      (is (thrown-with-msg? js/Error #"memfsDir" (stageFiles module bytes nil))))
    (testing "files must be an object map"
      (is (thrown-with-msg? js/Error #"files must be" (stageFiles module nil "/d")))
      (is (thrown-with-msg? js/Error #"files must be" (stageFiles module "a.dat" "/d"))))))

(tr/run-tests-and-exit! "net.willcohen.native.handler-fs-test")
