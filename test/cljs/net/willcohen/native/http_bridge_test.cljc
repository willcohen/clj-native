;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.http-bridge-test
  (:require [cljs.test :refer [deftest is]]
            ["ffi-wasm/http-bridge" :refer [createSyncFetch installXhrPolyfill shutdown]]
            ["ffi-wasm/test-runner" :as tr]
            ["node:worker_threads" :refer [Worker]]
            ["node:url" :refer [fileURLToPath pathToFileURL]]
            ["node:path" :refer [join dirname]]))

(def this-dir (dirname (fileURLToPath (.-url js/import.meta))))
;; Four levels up from test/cljs/net/willcohen/native is test/.
(def test-dir (join this-dir ".." ".." ".." ".."))

;; new Worker wants a URL object, not a file:// string.
(def worker-url (js/URL. (.resolve js/import.meta "ffi-wasm/fetch-worker")))
(def decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate.mjs"))))
(def broken-decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate-broken.mjs"))))
(def noexport-decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate-noexport.mjs"))))
(def hang-decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate-hang.mjs"))))
(def late-decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate-late.mjs"))))
(def crash-decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate-crash.mjs"))))
(def timer-decorate-url (.-href (pathToFileURL (join test-dir "fixtures" "http-bridge-decorate-timer.mjs"))))
(def server-worker-url (pathToFileURL (join test-dir "fixtures" "echo-server-worker.mjs")))

;; The server runs in its own worker thread, so it can accept connections
;; while the test thread blocks in Atomics.wait.
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

(defn echoed [res] (js/JSON.parse (decode (.-bodyBytes res))))

(defn sleep [ms] (js/Promise. (fn [resolve _reject] (js/setTimeout resolve ms))))

(defn ^:async with-echo-server
  "Call the async `f` with the base URL of a new echo server. Then release one
   fetch worker reference and stop the server."
  [f]
  (let [fixture (await (start-fixture))]
    (try
      (await (f (.-base fixture)))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(defn ^:async create-error-message [opts]
  (await (-> (createSyncFetch opts) (.then (fn [_] nil)) (.catch (fn [e] (.-message e))))))

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
          ;; The polyfill leaves .responseText empty, and `or` treats "" as
          ;; truthy, so test the length.
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
  (is (false? (await (shutdown))) "first shutdown with no worker resolves to false")
  (is (false? (await (shutdown))) "second shutdown is idempotent, also false"))

(deftest ^:async shutdown-is-reference-counted-across-consumers
  ;; Two libraries in one worker thread share one fetch worker. The short
  ;; requestTimeoutMs makes a worker killed on the first release fail in
  ;; about 5.5 s, not 40 s.
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
        (let [fetch-c (await (createSyncFetch #js {:workerUrl worker-url}))]
          (is (= 200 (.-status (fetch-c (str (.-base fixture) "/plain"))))
              "a consumer arriving after full teardown gets a fresh worker")
          (is (true? (await (shutdown)))
              "the fresh worker's sole reference terminates it")))
      (finally
        (await (stop-fixture fixture))))))

(deftest ^:async follows-redirects-like-the-jvm-and-browser-transports
  ;; The JVM and browser transports follow redirects. /redirect 302s to
  ;; /query?redirected=1.
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
  ;; A worker that serves after a failed decorator import drops auth.
  (let [broke-err (atom nil)
        noexp-err (atom nil)]
    (try (await (createSyncFetch #js {:workerUrl worker-url :decorateUrl broken-decorate-url}))
         (catch :default e (reset! broke-err (.-message e))))
    (try (await (createSyncFetch #js {:workerUrl worker-url :decorateUrl noexport-decorate-url}))
         (catch :default e (reset! noexp-err (.-message e))))
    ;; Match the message, so the 10 s readiness-timeout fallback cannot pass.
    (is (and @broke-err (.includes @broke-err "failed to initialize"))
        "rejects via the worker's explicit decorate-import error, not the readiness timeout")
    (is (and @noexp-err (.includes @noexp-err "failed to initialize"))
        "a non-function export also rejects via the explicit error")))

(deftest ^:async a-hanging-decorator-times-out-to-a-transport-failure
  ;; Without the worker's requestTimeoutMs abort, the caller blocks until its
  ;; 5.5 s backstop. The URL is live, so a hang fixture that stops hanging
  ;; gives 200, not 0.
  (let [fixture (await (start-fixture))
        sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                :decorateUrl hang-decorate-url
                                                :requestTimeoutMs 500}))]
    (try
      (let [t0 (js/Date.now)
            res (sync-fetch (str (.-base fixture) "/plain"))
            elapsed (- (js/Date.now) t0)]
        (is (= 0 (.-status res)) "a hanging decorator resolves to a status-0 transport failure")
        ;; Well below the 5.5 s caller-side backstop, so the worker-side abort fired.
        (is (< elapsed 2000)
            (str "the worker-side requestTimeoutMs abort fired (elapsed " elapsed "ms)")))
      (finally
        (await (shutdown))
        (await (stop-fixture fixture))))))

(deftest ^:async an-oversized-response-is-a-distinguishable-overflow-not-a-silent-failure
  ;; A body larger than dataBufferSize must fail visibly, not arrive
  ;; truncated as a success.
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
  ;; The timeout is an idle timer, reset on each chunk. The 1800 ms total must
  ;; stay above requestTimeoutMs, or the test proves nothing.
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
  ;; The caller abandons this 6.5 s transfer at its 5.5 s cap, and the worker's
  ;; idle timer never fires. The stale response must not answer the next request.
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

(deftest ^:async a-response-that-lands-after-the-caller-gave-up-does-not-lose-the-next-request
  (await (with-echo-server
           (fn ^:async late [base]
             (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                           :decorateUrl late-decorate-url
                                                           :requestTimeoutMs 250}))]
               (is (= 0 (.-status (sync-fetch (str base "/plain?n=1")))) "the caller gives up")
               (is (= "/plain?n=2" (.-url (echoed (sync-fetch (str base "/plain?n=2")))))))))))

(deftest ^:async a-second-consumer-with-another-decorator-is-refused
  ;; A shared fetch worker applies one decorator: a second one would drop its
  ;; auth, or send the first one's token to its hosts.
  (await (createSyncFetch #js {:workerUrl worker-url}))
  (is (.includes (await (create-error-message #js {:workerUrl worker-url :decorateUrl decorate-url}))
                 "decorateUrl"))
  (is (true? (await (shutdown))))
  (await (createSyncFetch #js {:workerUrl worker-url :decorateUrl decorate-url}))
  (is (.includes (await (create-error-message #js {:workerUrl worker-url})) "decorateUrl"))
  (is (true? (await (shutdown)))))

(deftest ^:async the-fetch-worker-runs-decorator-timers-between-requests
  (await (with-echo-server
           (fn ^:async timers [base]
             (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                           :decorateUrl timer-decorate-url}))]
               (await (sleep 50))
               (is (= "fired" (aget (.-headers (echoed (sync-fetch (str base "/plain")))) "x-timer"))))))))

(deftest ^:async a-response-larger-than-the-buffer-overflows-at-once
  ;; /empty has no body chunk, so only its headers overflow.
  (await (with-echo-server
           (fn ^:async overflow [base]
             (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                           :dataBufferSize 256
                                                           :requestTimeoutMs 500}))]
               (doseq [path ["/slow?chunks=26&delay=250&size=200" "/empty?pad=300"]]
                 (let [res (sync-fetch (str base path))]
                   (is (= 0 (.-status res)) path)
                   (is (.-overflow res) path))))))))

(deftest ^:async two-concurrent-creates-share-one-fetch-worker
  (await (with-echo-server
           (fn ^:async concurrent [base]
             (let [fetches (await (js/Promise.all
                                   #js [(createSyncFetch #js {:workerUrl worker-url :requestTimeoutMs 500})
                                        (createSyncFetch #js {:workerUrl worker-url :requestTimeoutMs 500})]))]
               (await (shutdown))
               (await (shutdown))
               (doseq [sync-fetch fetches]
                 (is (= 0 (.-status (sync-fetch (str base "/plain"))))
                     "the last release ended the only worker")))))))

(deftest ^:async a-byte-body-is-refused-on-node
  (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url}))]
    (is (thrown? js/TypeError
                 (sync-fetch "http://127.0.0.1:1/" #js {:method "POST" :body (js/Uint8Array. 3)})))
    (await (shutdown))))

(deftest ^:async a-crashed-fetch-worker-fails-fast-and-a-new-one-starts
  (await (with-echo-server
           (fn ^:async crashed [base]
             (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                           :decorateUrl crash-decorate-url
                                                           :requestTimeoutMs 500}))]
               (await (sleep 100))
               (let [t0  (js/Date.now)
                     res (sync-fetch (str base "/plain"))]
                 (is (= 0 (.-status res)))
                 (is (< (- (js/Date.now) t0) 100) "a dead worker answers at once"))
               (let [fresh (await (createSyncFetch #js {:workerUrl worker-url}))]
                 (is (= 200 (.-status (fresh (str base "/plain")))))
                 (is (false? (await (shutdown))) "the release of the crashed consumer keeps the new worker")
                 (is (= 200 (.-status (fresh (str base "/plain")))))))))))

(tr/run-tests-and-exit! "net.willcohen.native.http-bridge-test")
