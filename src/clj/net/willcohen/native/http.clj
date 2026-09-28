;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.http
  "Blocking java.net.http transport for wasm and native libraries that get
   their networking from the host. A consumer binding calls fetch or
   range-request from inside its own callback.

   :decorate in a request map is the auth hook. fetch applies it to the
   request first, and it may block, for example on a token refresh."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest
            HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]))

(set! *warn-on-reflection* true)

(def ^:private connect-timeout-ms
  "Connect timeout. java.net.http has none, so an unreachable host would
   block the caller thread. There is no default total timeout, because
   HttpRequest.timeout includes the body and would stop a slow large
   download."
  10000)

(defonce ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
        (.followRedirects HttpClient$Redirect/NORMAL)
        (.connectTimeout (Duration/ofMillis connect-timeout-ms))
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

(defn- body-publisher [body]
  (cond
    (nil? body) (HttpRequest$BodyPublishers/noBody)
    (bytes? body) (HttpRequest$BodyPublishers/ofByteArray body)
    :else (HttpRequest$BodyPublishers/ofString (str body))))

(defn- build-request ^HttpRequest [{:keys [url method headers body timeout-ms] :or {method :get}}]
  (let [^java.net.http.HttpRequest$Builder builder
        (reduce-kv (fn [^java.net.http.HttpRequest$Builder b k v] (.header b (name k) (str v)))
                   (cond-> (-> (HttpRequest/newBuilder) (.uri (URI. url)))
                     timeout-ms (.timeout (Duration/ofMillis (long timeout-ms))))
                   (or headers {}))
        m (-> method name str/upper-case)
        ^java.net.http.HttpRequest$Builder built
        (case m
          "GET" (.GET builder)
          "DELETE" (.DELETE builder)
          "POST" (.POST builder (body-publisher body))
          "PUT" (.PUT builder (body-publisher body))
          (.method builder m (body-publisher body)))]
    (.build built)))

(defn fetch
  "Send an HTTP request. Keys:
     :url         Required.
     :method      Keyword or string. Default :get.
     :headers     Map of name to value.
     :body        String or byte[].
     :timeout-ms  Optional total timeout.
     :decorate    Optional fn from request to request, applied first.

   Returns {:status :headers :body-bytes}, with headers as parse-headers
   gives them. On a transport failure, logs the cause at warn and returns
   {:status 0 :headers {} :body-bytes nil}."
  [{:keys [decorate url] :as request}]
  (try
    (let [request (cond-> (dissoc request :decorate)
                    decorate decorate)
          response (.send ^java.net.http.HttpClient @http-client
                          (build-request request)
                          (HttpResponse$BodyHandlers/ofByteArray))]
      {:status (.statusCode response)
       :headers (parse-headers (.headers response))
       :body-bytes (.body response)})
    (catch Exception e
      (log/warn e (str "HTTP request failed, returning status 0: " url))
      {:status 0 :headers {} :body-bytes nil})))

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
      (assoc :method :get
             :headers (assoc (or headers {})
                             "Range" (format "bytes=%d-%d" offset (+ (long offset) (long size) -1))))
      fetch))
