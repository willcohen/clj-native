;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.http
  "Blocking java.net.http transport for wasm and native libraries that get
   their networking from the host. A consumer binding calls fetch or
   range-request from inside its own callback."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(set! *warn-on-reflection* true)

(defonce ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
        (.followRedirects HttpClient$Redirect/NORMAL)
        ;; An unreachable host blocks forever with no connect timeout. A request
        ;; timeout would also cover the body and stop a slow download.
        (.connectTimeout (Duration/ofSeconds 10))
        (.build))))

(defn- parse-headers
  "Lower-case header names and join repeated values with \", \" (RFC 9110).
   Set-Cookie keeps its first value, because its values contain commas."
  [^java.net.http.HttpHeaders http-headers]
  (into {}
        (for [[k vs] (.map http-headers)]
          (let [lower (str/lower-case k)]
            [lower (if (= "set-cookie" lower)
                     (first vs)
                     (str/join ", " vs))]))))

(defn- build-request
  ^HttpRequest [{:keys [url headers]}]
  (.build ^java.net.http.HttpRequest$Builder
          (reduce-kv (fn [^java.net.http.HttpRequest$Builder b k v] (.header b (name k) (str v)))
                     (.uri (HttpRequest/newBuilder) (URI. url))
                     headers)))

(defn- transport-failure [url e]
  (log/warn e (str "HTTP request failed, returning status 0: " url))
  {:status 0 :headers {} :body-bytes nil})

(defn fetch
  "Send a GET of :url with the :headers map, and follow redirects. Returns
   {:status :headers :body-bytes}, with lower-case header names.
   On a transport failure, logs the cause at warn and returns
   {:status 0 :headers {} :body-bytes nil}."
  [{:keys [url] :as request}]
  (try
    (let [response (.send ^java.net.http.HttpClient @http-client
                          (build-request request)
                          (HttpResponse$BodyHandlers/ofByteArray))]
      {:status (.statusCode response)
       :headers (parse-headers (.headers response))
       :body-bytes (.body response)})
    (catch InterruptedException e
      ;; send clears the flag when it throws. Set it again for the caller,
      ;; after the log write, which an interrupted NIO channel would fail.
      (let [failure (transport-failure url e)]
        (.interrupt (Thread/currentThread))
        failure))
    (catch Exception e
      (transport-failure url e))))

(defn range-request
  "fetch :size bytes of :url from :offset with a Range GET. Takes the fetch
   request map plus :offset and :size. Throws unless :offset >= 0 and
   :size >= 1, because a server can answer a reversed range with 200 and
   the whole body."
  [{:keys [offset size headers] :as request}]
  (when-not (and (number? offset) (number? size) (nat-int? (long offset)) (pos? (long size)))
    (throw (ex-info "range-request needs a non-negative :offset and a :size of at least 1"
                    {:url (:url request) :offset offset :size size})))
  (-> request
      (dissoc :offset :size)
      (assoc :headers (assoc headers
                             "Range" (format "bytes=%d-%d" (long offset) (+ (long offset) (long size) -1))))
      fetch))
