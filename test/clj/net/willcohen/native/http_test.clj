;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.http-test
  "JVM tests for the platform HTTP transport. A local HttpServer fixture echoes
   request headers back as response headers and honors Range requests, so the
   assertions can inspect exactly what fetch/range-request sent, including a
   header injected by the :decorate auth seam."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [net.willcohen.native.http :as http])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.net InetSocketAddress]
           [java.util Arrays]))

;; 10-byte payload the fixture serves; range slices index into it.
(def ^:private payload (.getBytes "ABCDEFGHIJ"))

(defn- handle-exchange [^HttpExchange exchange]
  (let [req-headers (.getRequestHeaders exchange)
        resp-headers (.getResponseHeaders exchange)
        path (.getPath (.getRequestURI exchange))
        range (.getFirst req-headers "Range")]
    (when-let [v (.getFirst req-headers "X-Injected")]
      (.set resp-headers "X-Injected-Echoed" v))
    ;; Two field lines under one name, for both the combinable case and the
    ;; one RFC 9110 excludes. The Set-Cookie value carries a comma of its own,
    ;; which is exactly why a comma join would corrupt it.
    (when (= path "/repeated")
      (doto resp-headers
        (.add "Link" "<https://a.example>; rel=next")
        (.add "Link" "<https://b.example>; rel=prev")
        (.add "Set-Cookie" "a=1; Expires=Wed, 21 Oct 2026 07:28:00 GMT")
        (.add "Set-Cookie" "b=2")))
    (if (and range (= path "/range"))
      (let [[_ a b] (re-matches #"bytes=(\d+)-(\d+)" range)
            a (Integer/parseInt a)
            b (Integer/parseInt b)
            slice (Arrays/copyOfRange payload a (inc b))]
        (.sendResponseHeaders exchange 206 (alength slice))
        (doto (.getResponseBody exchange) (.write slice) (.close)))
      (do
        (.sendResponseHeaders exchange 200 (alength payload))
        (doto (.getResponseBody exchange) (.write payload) (.close))))))

(def ^:private ^:dynamic *base* nil)

(defn- with-server [f]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext server "/" (reify HttpHandler (handle [_ ex] (handle-exchange ex))))
    (.setExecutor server nil)
    (.start server)
    (try
      (binding [*base* (str "http://127.0.0.1:" (.getPort (.getAddress server)))]
        (f))
      (finally
        (.stop server 0)))))

(use-fixtures :each with-server)

(deftest fetch-returns-status-headers-body
  (let [res (http/fetch {:url (str *base* "/plain")})]
    (is (= 200 (:status res)))
    (is (map? (:headers res)))
    (is (= "ABCDEFGHIJ" (String. ^bytes (:body-bytes res))))))

(deftest repeated-header-lines-combine-except-set-cookie
  ;; RFC 9110 lets a recipient join repeated field lines of one name with
  ;; commas, and names Set-Cookie as the exception. Keeping only the first
  ;; value, which is what every name did before, silently drops the second
  ;; half of a paged Link header.
  (let [h (:headers (http/fetch {:url (str *base* "/repeated")}))]
    (testing "a repeated Link keeps both entries, in order"
      (is (= "<https://a.example>; rel=next, <https://b.example>; rel=prev"
             (get h "link"))))
    (testing "Set-Cookie keeps the first value rather than a corrupt comma join"
      (is (= "a=1; Expires=Wed, 21 Oct 2026 07:28:00 GMT" (get h "set-cookie"))))))

(deftest range-request-sends-range-and-returns-slice
  (let [res (http/range-request {:url (str *base* "/range") :offset 2 :size 3})]
    (is (= 206 (:status res)))
    ;; indices 2,3,4 of "ABCDEFGHIJ"
    (is (= "CDE" (String. ^bytes (:body-bytes res))))))

(deftest range-request-rejects-a-size-below-one
  ;; bytes=2-1 is a reversed range. A server may answer it with 200 and the
  ;; whole body, and the PROJ callbacks in clj-proj accept 200 as a successful
  ;; range read, so the caller would write the whole file where it asked for
  ;; nothing. Failing loudly here is the only way that stays visible.
  (testing "a zero size throws rather than emitting a reversed range"
    (is (thrown? clojure.lang.ExceptionInfo
                 (http/range-request {:url (str *base* "/range") :offset 2 :size 0}))))
  (testing "a negative size throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (http/range-request {:url (str *base* "/range") :offset 2 :size -3}))))
  (testing "a negative offset throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (http/range-request {:url (str *base* "/range") :offset -1 :size 3}))))
  (testing "a size of exactly one is the smallest legal request"
    (let [res (http/range-request {:url (str *base* "/range") :offset 2 :size 1})]
      (is (= 206 (:status res)))
      (is (= "C" (String. ^bytes (:body-bytes res)))))))

(deftest fetch-reports-a-transport-failure-as-status-0
  ;; Coverage of behavior that already held, not a pin on a fix. The catch it
  ;; exercises now also logs the cause, which nothing here asserts: the log
  ;; call is the only record a caller gets, and status 0 is what it reads.
  (testing "an unreachable host is status 0 with an empty header map"
    ;; Port 1 on the loopback interface refuses immediately, so this exercises
    ;; the catch without waiting out the connect timeout.
    (let [res (http/fetch {:url "http://127.0.0.1:1/nope"})]
      (is (= 0 (:status res)))
      (is (= {} (:headers res)))
      (is (nil? (:body-bytes res))))))

(deftest decorate-injects-a-header
  (testing "the :decorate hook mutates the request before dispatch"
    (let [res (http/fetch {:url (str *base* "/plain")
                           :decorate (fn [req]
                                       (update req :headers assoc "X-Injected" "tok"))})]
      (is (= 200 (:status res)))
      (is (= "tok" (get-in res [:headers "x-injected-echoed"]))))))
