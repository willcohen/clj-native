;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.build-test
  "Tests of the babashka build helpers. bb test:bb runs them."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [babashka.tasks :as tasks]
            [clojure.test :refer [deftest is testing]]
            [net.willcohen.native.build :as nb]
            [org.httpkit.server :as http]))

(def ^:private body "hello")

(def ^:private body-sha256
  "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")

(defn- with-server
  "Call `f` with the URL of a local server that answers each request with
   `body`."
  [f]
  (let [stop (http/run-server (fn [_] {:status 200 :body body})
                              {:ip "127.0.0.1" :port 0})]
    (try (f (str "http://127.0.0.1:" (:local-port (meta stop)) "/a.tar.gz"))
         (finally (stop)))))

(deftest download-archive-checks-the-sha256
  (with-server
    (fn [url]
      (let [dest (str (fs/path (fs/create-temp-dir) "a.tar.gz"))]
        (testing "the expected sha256 keeps the file"
          (nb/download-archive url dest {:sha256 body-sha256})
          (is (= body (slurp dest))))
        (testing "a partial file from an earlier run fails the check and goes"
          (spit dest "hel")
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"sha256"
                                (nb/download-archive url dest {:sha256 body-sha256})))
          (is (not (fs/exists? dest))))
        (testing "the sha256 compares in any case"
          (nb/download-archive url dest {:sha256 (str/upper-case body-sha256)})
          (is (= body (slurp dest))))))))

(defn- with-truncating-server
  "Call `f` with the URL of a local server that sends 5 of the 10 bytes its
   Content-Length promises, then closes."
  [f]
  (let [ss  (java.net.ServerSocket. 0 1 (java.net.InetAddress/getLoopbackAddress))
        srv (future
              (with-open [sock (.accept ss)]
                (let [in (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream sock)))]
                  (loop [] (when (seq (.readLine in)) (recur)))
                  (doto (.getOutputStream sock)
                    (.write (.getBytes "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nhello"))
                    (.flush)))))]
    (try (f (str "http://127.0.0.1:" (.getLocalPort ss) "/a.tar.gz"))
         (finally (future-cancel srv) (.close ss)))))

(deftest download-archive-leaves-no-partial-file
  ;; A partial file would pass as the archive on each later run.
  (with-truncating-server
    (fn [url]
      (let [dest (str (fs/path (fs/create-temp-dir) "a.tar.gz"))]
        (is (thrown? Exception (nb/download-archive url dest)))
        (is (not (fs/exists? dest)))))))

(deftest extract-archive-leaves-no-partial-dir
  ;; A partial dir would pass as the source tree on each later run.
  (let [base    (fs/create-temp-dir)
        archive (fs/path base "p.tar.gz")
        rnd     (java.util.Random. 1)]
    (fs/create-dirs (fs/path base "p"))
    (doseq [f ["a" "b"]
            :let [b (byte-array 200000)]]
      (.nextBytes rnd b)
      (fs/write-bytes (fs/path base "p" f) b))
    (tasks/shell {:dir (str base)} "tar" "czf" "p.tar.gz" "p")
    (let [whole (fs/read-all-bytes archive)]
      (fs/write-bytes archive (java.util.Arrays/copyOf whole (quot (count whole) 2))))
    (fs/delete-tree (fs/path base "p"))
    (is (thrown? Exception (nb/extract-archive "p.tar.gz" "p" base)))
    (is (not (fs/exists? (fs/path base "p"))))))

(deftest build-once-runs-again-when-the-args-change
  (let [out    (str (fs/path (fs/create-temp-dir) "lib" "libx.a"))
        runs   (atom 0)
        build! #(do (swap! runs inc)
                    (fs/create-dirs (fs/parent out))
                    (spit out "x"))]
    (nb/build-once! out {:cflags "-O2"} build!)
    (nb/build-once! out {:cflags "-O2"} build!)
    (is (= 1 @runs) "the same args keep the output")
    (nb/build-once! out {:cflags "-O3"} build!)
    (is (= 2 @runs) "new args build again")
    (fs/delete out)
    (nb/build-once! out {:cflags "-O3"} build!)
    (is (= 3 @runs) "a missing output builds again")
    (testing "an old output does not pass for a build that makes none"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"made no"
                            (nb/build-once! out {:cflags "-O0"} (fn []))))
      (is (not (fs/exists? (str out ".build-args.edn"))))
      (nb/build-once! out {:cflags "-O0"} build!)
      (is (= 4 @runs)))))

(deftest build-once-rejects-args-that-do-not-print-as-edn
  (let [out (str (fs/path (fs/create-temp-dir) "libx.a"))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"EDN"
                          (nb/build-once! out {:dir (fs/path "x")} #(spit out "x"))))))

(deftest autotools-rejects-cflags-with-no-optimization-level
  (let [existing (str (fs/create-temp-file))
        opts     {:type :native :build-dir "/nonexistent" :install-dir "/nonexistent"
                  :configure-args [] :skip-if-exists existing}]
    (doseq [flags ["" "-DSQLITE_ENABLE_RTREE"]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"-O"
                            (nb/build-autotools-library (assoc opts :env {"CFLAGS" flags "CXXFLAGS" "-O2"})))
          (str "CFLAGS \"" flags "\" replaces the -O2 of configure")))
    (doseq [flags ["-O2 -DX" "-DX -Os" "-O0" "-Og" "-Ofast -DX"]]
      (is (nil? (nb/build-autotools-library (assoc opts :env {"CFLAGS" flags "CXXFLAGS" "-O2"})))
          (str "CFLAGS \"" flags "\" has a level")))))

(deftest cflags-check-reads-the-inherited-environment
  ;; A nix shell can export CFLAGS, and configure then sees that value.
  (let [check! #'nb/check-cflags!]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"CFLAGS \"-fPIC\""
                          (check! {} {"CFLAGS" "-fPIC"})))
    (is (nil? (check! {} {"CFLAGS" "-O2 -fPIC"})))
    (is (nil? (check! {"CFLAGS" "-O2"} {"CFLAGS" "-fPIC"})) "env wins over the inherited value")))
