;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.http
  "Platform HTTP transport for wasm and native libraries. These libraries get
   their networking from the host. One example is a C runtime that issues
   blocking range GETs or fetch callbacks.

   This namespace uses java.net.http only. It does requests and nothing else.

   Callback marshaling stays in each consumer binding. That binding calls fetch
   or range-request from inside its own callback. The callback is a dt-ffi
   upcall stub on the FFI path, or a GraalVM ProxyExecutable on the polyglot
   path. Refer to net.willcohen.native.callbacks for the FFI path.

   A request is a map {:url :method :headers :body :decorate}. :decorate is the
   auth seam. It is an optional function from request to request, and fetch
   applies it before dispatch. Thus :decorate can add auth headers. It can also
   block on a token refresh, because the JVM caller is a real thread. The JS
   side blocks a caller with Atomics instead."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest
            HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]))

(set! *warn-on-reflection* true)

(def ^:private connect-timeout-ms
  "Cap on the time to make the TCP/TLS connection. java.net.http has no connect
   timeout by default. Thus an unreachable host blocks the caller thread while
   the OS continues to retry. The caller here is often a thread that a wasm
   runtime waits for.

   This namespace has no matching default for the whole request, and that is
   deliberate. HttpRequest.timeout counts body reception too. Thus a default
   would stop a healthy multi-megabyte download on a slow link. The JS bridge
   carries an idle timer to prevent that exact bug. A caller that wants a total
   cap passes :timeout-ms."
  10000)

(defonce ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
        (.followRedirects HttpClient$Redirect/NORMAL)
        (.connectTimeout (Duration/ofMillis connect-timeout-ms))
        (.build))))

(defn- parse-headers
  "Lower-case every header name. Then combine repeated field lines.

   RFC 9110 lets a recipient join repeated field lines of one name. The joined
   form is a single comma-separated value. Thus a repeated Link or Vary keeps
   all of its entries.

   Set-Cookie is the documented exception. Its values contain commas of their
   own, and a comma join corrupts them. Thus Set-Cookie keeps the first value
   only. That is what every name did before this change."
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
  "Send an HTTP request. The request map has these keys:
     :url         Required.
     :method      A keyword or a string. The default is :get.
     :headers     A map of name to value.
     :body        A String or a byte[], for POST and PUT.
     :timeout-ms  An optional total cap. Refer to connect-timeout-ms.
     :decorate    An optional function from request to request. fetch applies
                  it first.

   Returns {:status :headers :body-bytes}. On a transport failure, returns
   {:status 0 :headers {} :body-bytes nil} and logs the cause at warn level.

   A repeated header becomes one comma-separated value, because the caller is
   a C library that reads one string for each name. Set-Cookie is the
   exception, and it keeps its first value only. Refer to parse-headers.

   Status 0 is the one signal a caller gets for every transport failure. Thus
   the log line is the only place where the cause survives."
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
  "Send a GET to :url with a `Range: bytes=offset-(offset+size-1)` header.
   Takes the same request map as fetch, and also :offset and :size. :decorate
   still applies.

   Throws on a :size below 1. Without that check, the arithmetic emits a
   reversed range such as `bytes=100-99`. A server can answer such a range
   with status 200 and the whole body. The caller then sees a success status
   with bytes that it never requested. That result is worse than a failure."
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
