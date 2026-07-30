;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
;;
;; cljs.test suite for the http-bridge: a synchronous HTTP transport for
;; wasm libraries whose C runtime issues blocking XHR (e.g. emscripten FETCH
;; downloads or a synchronous fetch callback). On Node the sync round-trip crosses a fetch worker
;; over SharedArrayBuffer + Atomics; the decorator seam (auth) runs inside
;; that worker. The fixture HTTP server runs in its own worker thread so it
;; can accept the fetch worker's connection while the test thread is blocked
;; in Atomics.wait.
;;
;; Ported from the retired node:test suite test/http-bridge.test.mjs.

(ns net.willcohen.native.http-bridge-test
  (:require [cljs.test :refer [deftest is]]
            ["ffi-wasm/http-bridge" :refer [createSyncFetch installXhrPolyfill shutdown]]
            ["ffi-wasm/test-runner" :as tr]
            ["node:worker_threads" :refer [Worker]]
            ["node:url" :refer [fileURLToPath pathToFileURL]]
            ["node:path" :refer [join dirname]]))

(def this-dir (dirname (fileURLToPath (.-url js/import.meta))))
;; test/cljs/net/willcohen/native -> up four (native, willcohen, net, cljs)
;; to test/, where the shared fixtures live.
(def test-dir (join this-dir ".." ".." ".." ".."))

;; The fetch worker ships as a package export; resolve it by name so the
;; URL is independent of this test's location in the tree. new Worker wants
;; a URL object (or ./-relative path), not a bare file:// string, so wrap
;; the resolved href in URL.
(def worker-url (js/URL. (.resolve js/import.meta "ffi-wasm/fetch-worker")))
(def decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate.mjs"))))
(def broken-decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate-broken.mjs"))))
(def noexport-decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate-noexport.mjs"))))
(def hang-decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate-hang.mjs"))))
(def server-worker-url (pathToFileURL (join test-dir "fixtures" "echo-server-worker.mjs")))

(defn ^:async start-fixture []
  (js/Promise.
   (fn [resolve _reject]
     (let [worker (Worker. server-worker-url)]
       (.on worker "message"
            (fn [msg]
              (when (= (.-type msg) "ready")
                (resolve #js {:worker worker
                              :base (str "http://127.0.0.1:" (.-port msg))}))))))))

(defn ^:async stop-fixture [fixture]
  (js/Promise.
   (fn [resolve _reject]
     (.on (.-worker fixture) "message"
          (fn [msg]
            (when (= (.-type msg) "closed")
              (.then (.terminate (.-worker fixture)) resolve))))
     (.postMessage (.-worker fixture) #js {:cmd "close"}))))

(defn decode [bytes] (.decode (js/TextDecoder.) bytes))

(deftest ^:async round-trips-status-headers-body-and-applies-worker-decorator
  (let [fixture (await (start-fixture))]
    (try
      (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                    :decorateUrl decorate-url}))
            res (sync-fetch (str (.-base fixture) "/query?f=json")
                            #js {:headers #js {"x-orig" "client"}})]
        (is (= 200 (.-status res)))
        (is (= "yes" (aget (.-headers res) "x-fixture")))
        (let [echoed (js/JSON.parse (decode (.-bodyBytes res)))]
          (is (= "/query?f=json" (.-url echoed)))
          (is (= "GET" (.-method echoed)))
          (is (= "client" (aget (.-headers echoed) "x-orig")))
          ;; The decorator ran inside the fetch worker and injected this header.
          (is (= "bridge-decorator" (aget (.-headers echoed) "x-injected")))))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(deftest ^:async without-a-decorator-is-identity
  (let [fixture (await (start-fixture))]
    (try
      (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url}))
            res (sync-fetch (str (.-base fixture) "/plain"))]
        (is (= 200 (.-status res)))
        (let [echoed (js/JSON.parse (decode (.-bodyBytes res)))]
          (is (= js/undefined (aget (.-headers echoed) "x-injected")))))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(deftest ^:async installXhrPolyfill-wires-a-global-synchronous-XMLHttpRequest
  (let [fixture (await (start-fixture))
        saved-xhr (.-XMLHttpRequest js/globalThis)]
    (try
      (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url}))]
        (await (installXhrPolyfill #js {:syncFetch sync-fetch}))
        (let [xhr (new js/globalThis.XMLHttpRequest)]
          (.open xhr "GET" (str (.-base fixture) "/xhr") false)
          (.setRequestHeader xhr "x-from-xhr" "1")
          (.send xhr)
          (is (= 200 (.-status xhr)))
          ;; The polyfill always returns bytes in .response and leaves
          ;; .responseText empty; prefer responseText only when non-empty
          ;; (JS `||` would fall through on "", but cljs `or` treats "" as
          ;; truthy, so guard on length explicitly).
          (let [rt (.-responseText xhr)
                echoed (js/JSON.parse (if (and rt (pos? (.-length rt)))
                                        rt
                                        (decode (js/Uint8Array. (.-response xhr)))))]
            (is (= "/xhr" (.-url echoed)))
            (is (= "1" (aget (.-headers echoed) "x-from-xhr"))))))
      (finally
        (set! (.-XMLHttpRequest js/globalThis) saved-xhr)
        (await (shutdown))
        (await (stop-fixture fixture))))))

(deftest ^:async shutdown-is-idempotent-and-safe-with-no-worker-spawned
  ;; With no worker ever spawned, shutdown short-circuits on the null
  ;; workerState and resolves to false (nothing terminated); a second call
  ;; must do the same (idempotent) rather than throw on the already-null
  ;; state.
  (is (false? (await (shutdown))) "first shutdown with no worker resolves to false")
  (is (false? (await (shutdown))) "second shutdown is idempotent, also false"))

(deftest ^:async shutdown-is-reference-counted-across-consumers
  ;; Two libraries in one worker thread share the one fetch worker (the
  ;; joint pool loads both handler modules into the same thread, and each
  ;; handler's teardown calls shutdown). The first release must NOT kill
  ;; the transport out from under the still-live consumer; the last
  ;; release terminates the worker. requestTimeoutMs shrinks the caller's
  ;; Atomics.wait backstop so a regression to terminate-on-first-release
  ;; fails in ~5.5s (status 0) instead of ~40s.
  (let [fixture (await (start-fixture))]
    (try
      (let [fetch-a (await (createSyncFetch #js {:workerUrl worker-url
                                                 :requestTimeoutMs 500}))
            fetch-b (await (createSyncFetch #js {:workerUrl worker-url}))]
        (is (= 200 (.-status (fetch-a (str (.-base fixture) "/plain"))))
            "consumer A's transport works before any release")
        (is (false? (await (shutdown)))
            "the first release keeps the shared worker up")
        (is (= 200 (.-status (fetch-b (str (.-base fixture) "/plain"))))
            "consumer B's transport survives consumer A's release")
        (is (true? (await (shutdown)))
            "the last release terminates the worker")
        ;; Fully reset: a fresh consumer spawns a new worker and serves.
        (let [fetch-c (await (createSyncFetch #js {:workerUrl worker-url}))]
          (is (= 200 (.-status (fetch-c (str (.-base fixture) "/plain"))))
              "a consumer arriving after full teardown gets a fresh worker")
          (is (true? (await (shutdown)))
              "the fresh worker's sole reference terminates it")))
      (finally
        (await (stop-fixture fixture))))))

(deftest ^:async follows-redirects-like-the-jvm-and-browser-transports
  ;; The fetch worker uses redirect:'follow', matching the JVM (HttpClient
  ;; NORMAL) and browser XHR paths. The prior http/https.request path returned
  ;; the empty 3xx body instead. /redirect 302s to /query?redirected=1.
  (let [fixture (await (start-fixture))]
    (try
      (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url}))
            res (sync-fetch (str (.-base fixture) "/redirect"))]
        (is (= 200 (.-status res)) "the worker followed the 302 to its 200 target")
        (let [echoed (js/JSON.parse (decode (.-bodyBytes res)))]
          (is (= "/query?redirected=1" (.-url echoed))
              "the final response is the redirect target, not the empty 3xx")))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(deftest ^:async decorator-import-failure-rejects-rather-than-serving-unauthenticated
  ;; A decorateUrl that throws on import, or resolves to a non-function, is an
  ;; auth drop if the worker becomes ready anyway. createSyncFetch must reject in
  ;; both cases rather than fall back to serving requests undecorated.
  (let [broke-err (atom nil)
        noexp-err (atom nil)]
    (try (await (createSyncFetch #js {:workerUrl worker-url :decorateUrl broken-decorate-url}))
         (catch :default e (reset! broke-err (.-message e))))
    (try (await (createSyncFetch #js {:workerUrl worker-url :decorateUrl noexport-decorate-url}))
         (catch :default e (reset! noexp-err (.-message e))))
    ;; Assert the message, not just that it rejected: the worker's explicit
    ;; decorate-error path reads "...failed to initialize: decorate import
    ;; failed: ...". Matching that string excludes a false pass via the 10s
    ;; readiness-timeout fallback ("did not become ready within 10000ms").
    (is (and @broke-err (.includes @broke-err "failed to initialize"))
        "rejects via the worker's explicit decorate-import error, not the readiness timeout")
    (is (and @noexp-err (.includes @noexp-err "failed to initialize"))
        "a non-function export also rejects via the explicit error")))

(deftest ^:async a-hanging-decorator-times-out-to-a-transport-failure
  ;; The decorator never resolves; the worker's requestTimeoutMs must abort it so
  ;; the blocked Atomics.wait caller unblocks with status 0 rather than
  ;; deadlocking forever. requestTimeoutMs is shrunk to keep the test quick.
  ;; Points at the LIVE fixture (not a dead port): if the hang fixture regressed
  ;; to a normal decorator, the fetch would return 200, not 0.
  (let [fixture (await (start-fixture))
        sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                :decorateUrl hang-decorate-url
                                                :requestTimeoutMs 500}))]
    (try
      (let [t0 (js/Date.now)
            res (sync-fetch (str (.-base fixture) "/plain"))
            elapsed (- (js/Date.now) t0)]
        (is (= 0 (.-status res)) "a hanging decorator resolves to a status-0 transport failure")
        ;; Prove the WORKER-side 500ms abort fired, not the ~5.5s caller-side
        ;; Atomics.wait backstop it exists to protect: elapsed must be far below
        ;; the backstop. Removing the worker-side abort would push this to ~5.5s.
        (is (< elapsed 2000)
            (str "the worker-side requestTimeoutMs abort fired (elapsed " elapsed "ms)")))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(deftest ^:async an-oversized-response-is-a-distinguishable-overflow-not-a-silent-failure
  ;; dataBufferSize is shrunk so a modest body overflows the transport buffer.
  ;; The result is status 0 with overflow:true -- distinct from a network error,
  ;; and not a silently-truncated body masquerading as success.
  (let [fixture (await (start-fixture))]
    (try
      (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url :dataBufferSize 4096}))
            res (sync-fetch (str (.-base fixture) "/large?bytes=8192"))]
        (is (= 0 (.-status res)) "overflow surfaces as a transport-failure status")
        (is (.-overflow res) "overflow is flagged distinctly from a network error"))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(deftest ^:async follows-a-multi-hop-redirect-chain
  ;; Two 302 hops: /redirect-chain -> /redirect-chain-2 -> /query?redirected=2.
  ;; A regression capping at the first hop would land on the wrong target.
  (let [fixture (await (start-fixture))]
    (try
      (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url}))
            res (sync-fetch (str (.-base fixture) "/redirect-chain"))]
        (is (= 200 (.-status res)) "followed both hops to the terminal 200")
        (let [echoed (js/JSON.parse (decode (.-bodyBytes res)))]
          (is (= "/query?redirected=2" (.-url echoed)) "landed on the terminal target")))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(deftest ^:async a-slow-but-steady-download-is-not-killed-by-the-request-timeout
  ;; 6 chunks 300ms apart = ~1800ms total, exceeding the 1500ms
  ;; requestTimeoutMs. The timeout is an IDLE timer reset on each chunk, not a
  ;; whole-request cap, so a healthy slow transfer completes (matching the
  ;; JVM/browser transports). A whole-request cap would abort it at 1500ms and
  ;; return status 0.
  ;;
  ;; Raising requestTimeoutMs alone voids the test, because the total has to
  ;; stay above it. The leftover, requestTimeoutMs minus delay, is how long a
  ;; single gap may stall before the idle timer fires.
  (let [fixture (await (start-fixture))]
    (try
      (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                    :requestTimeoutMs 1500}))
            res (sync-fetch (str (.-base fixture) "/slow?chunks=6&delay=300"))]
        (is (= 200 (.-status res)) "a steady transfer longer than requestTimeoutMs still completes")
        (is (= 60 (.-length (.-bodyBytes res))) "full body received, not truncated by an abort"))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(deftest ^:async a-caller-timeout-does-not-mispair-the-next-request-with-a-stale-response
  ;; The worker's timeout is an idle timer (unbounded total transfer); the
  ;; caller's Atomics.wait is a fixed wall-clock cap of requestTimeoutMs + 5000.
  ;; A transfer living between the two -- 26 chunks 250ms apart is 6.5s total
  ;; with 250ms gaps that never trip the 500ms idle timer, against a 5.5s caller
  ;; cap -- makes the caller abandon a request the worker is still serving.
  ;;
  ;; Before the generation check, that abandoned response landed in the shared
  ;; buffers and was read as the answer to the NEXT request: status 200 carrying
  ;; another URL's bytes, undetectable by the caller. The follow-up request must
  ;; get its OWN response.
  (let [fixture (await (start-fixture))]
    (try
      (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                    :requestTimeoutMs 500}))
            abandoned (sync-fetch (str (.-base fixture) "/slow?chunks=26&delay=250"))]
        (is (= 0 (.-status abandoned)) "the caller gives up on the over-cap transfer")
        ;; Issued while the worker is still streaming the abandoned request.
        (let [res (sync-fetch (str (.-base fixture) "/plain"))
              body (decode (.-bodyBytes res))]
          (is (= 200 (.-status res)) "the follow-up request completes")
          (is (not (.startsWith body "y"))
              "the follow-up did not receive the abandoned /slow payload")
          (let [echoed (js/JSON.parse body)]
            (is (= "/plain" (.-url echoed))
                "the follow-up received its own response, not a stale one"))))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(tr/run-tests-and-exit! "net.willcohen.native.http-bridge-test")
