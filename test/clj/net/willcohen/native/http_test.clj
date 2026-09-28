;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.http-test
  "JVM tests for the platform HTTP transport, against a local HttpServer that
   echoes request headers back and honors Range requests."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.tools.logging.test :as lt]
            [net.willcohen.native.http :as http])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.net ConnectException InetSocketAddress]
           [java.util Arrays]))

(def ^:private payload (.getBytes "ABCDEFGHIJ"))

(defn- handle-exchange [^HttpExchange exchange]
  (let [req-headers (.getRequestHeaders exchange)
        resp-headers (.getResponseHeaders exchange)
        path (.getPath (.getRequestURI exchange))
        range (.getFirst req-headers "Range")]
    ;; The Set-Cookie value holds its own comma, so a comma join corrupts it.
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
    (is (= "10" (get (:headers res) "content-length")))
    (is (= "ABCDEFGHIJ" (String. ^bytes (:body-bytes res))))))

(deftest repeated-header-lines-combine-except-set-cookie
  ;; RFC 9110 lets a recipient join repeated field lines with commas, except
  ;; Set-Cookie. Keeping only the first value drops half of a paged Link header.
  (let [h (:headers (http/fetch {:url (str *base* "/repeated")}))]
    (testing "a repeated Link keeps both entries, in order"
      (is (= "<https://a.example>; rel=next, <https://b.example>; rel=prev"
             (get h "link"))))
    (testing "Set-Cookie keeps its first value"
      (is (= "a=1; Expires=Wed, 21 Oct 2026 07:28:00 GMT" (get h "set-cookie"))))))

(deftest range-request-returns-the-slice
  ;; Size 1 is the smallest range; a double offset once threw in format.
  (doseq [[offset size want] [[2 3 "CDE"] [2 1 "C"] [2.0 3 "CDE"]]]
    (let [res (http/range-request {:url (str *base* "/range") :offset offset :size size})]
      (is (= 206 (:status res)))
      (is (= want (String. ^bytes (:body-bytes res)))))))

(deftest range-request-rejects-a-bad-range
  ;; A server may answer the reversed range bytes=2-1 with 200 and the whole
  ;; body, which the clj-proj PROJ callbacks accept as a range read.
  (doseq [[offset size] [[2 0] [2 -3] [-1 3]]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (http/range-request {:url (str *base* "/range") :offset offset :size size}))
        (str "offset " offset ", size " size))))

(deftest fetch-keeps-the-interrupt-of-the-caller
  (lt/with-log
    ;; Read the flag before `is`, whose STM report counter clears it.
    (let [res  (do (.interrupt (Thread/currentThread))
                   (http/fetch {:url (str *base* "/plain")}))
          flag (Thread/interrupted)]
      (is (= 0 (:status res)))
      (is flag)
      (is (lt/logged? 'net.willcohen.native.http :warn InterruptedException
                      #"HTTP request failed")))))

(deftest fetch-reports-a-transport-failure-as-status-0
  (testing "an unreachable host is status 0 with an empty header map"
    ;; Port 1 on loopback refuses at once, with no wait for the connect timeout.
    (lt/with-log
      (let [res (http/fetch {:url "http://127.0.0.1:1/nope"})]
        (is (= 0 (:status res)))
        (is (= {} (:headers res)))
        (is (nil? (:body-bytes res)))
        (is (lt/logged? 'net.willcohen.native.http :warn ConnectException
                        #"HTTP request failed"))))))
