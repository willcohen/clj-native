;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.http-bridge-test
  (:require [cljs.test :refer [deftest is]]
            ["ffi-wasm/http-bridge" :refer [createSyncFetch installXhrPolyfill shutdown]]
            ["ffi-wasm/test-runner" :as tr]
            ["node:events" :refer [once]]
            ["node:worker_threads" :refer [Worker]]))

;; new Worker takes a URL object.
(def worker-url (js/URL. (.resolve js/import.meta "ffi-wasm/fetch-worker")))
(defn fixture-url [file] (js/URL. (str "../../../../fixtures/" file) (.-url js/import.meta)))
(def server-worker-url (fixture-url "echo-server-worker.mjs"))
(def hang-decorate-url (.-href (fixture-url "http-bridge-decorate-hang.mjs")))
(def late-decorate-url (.-href (fixture-url "http-bridge-decorate-late.mjs")))

(defn module-url [source] (str "data:text/javascript," (js/encodeURIComponent source)))
(def decorate-url
  (module-url "export default async (r) => ({...r, headers: {...r.headers, 'x-injected': 'bridge-decorator'}});"))
(def broken-decorate-url (module-url "throw new Error('decorator import blew up');"))
(def noexport-decorate-url (module-url "export const notADecorator = 42;"))
;; The unhandled rejection ends the fetch worker after it reports ready.
(def crash-decorate-url
  (module-url (str "setTimeout(() => Promise.reject(new Error('token refresh failed')), 0);"
                   "export default (r) => r;")))
;; The timer fires before a request only if the fetch worker idles on its event loop.
(def timer-decorate-url
  (module-url (str "let timer = 'pending'; setTimeout(() => { timer = 'fired'; }, 0);"
                   "export default (r) => ({...r, headers: {...r.headers, 'x-timer': timer}});")))

(defn decode [bytes] (.decode (js/TextDecoder.) bytes))

(defn echoed [res] (js/JSON.parse (decode (.-bodyBytes res))))

(defn sleep [ms] (js/Promise. (fn [resolve _reject] (js/setTimeout resolve ms))))

;; The server runs in its own worker thread, so it can accept connections
;; while the test thread blocks in Atomics.wait.
(defn ^:async with-echo-server
  "Call the async `f` with the base URL of a new echo server. Then release one
   fetch worker reference and stop the server."
  [f]
  (let [server (Worker. server-worker-url)
        [ready] (await (once server "message"))]
    (try
      (await (f (str "http://127.0.0.1:" (.-port ready))))
      (finally
        (await (shutdown))
        (await (.terminate server))))))

(defn ^:async create-error-message [opts]
  (await (-> (createSyncFetch opts) (.then (fn [_] nil)) (.catch (fn [e] (.-message e))))))

(deftest ^:async round-trips-status-headers-body-and-applies-worker-decorator
  (await (with-echo-server
           (fn ^:async round-trip [base]
             (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                           :decorateUrl decorate-url}))
                   res (sync-fetch (str base "/query?f=json")
                                   #js {:headers #js {"x-orig" "client"}})
                   body (echoed res)]
               (is (= 200 (.-status res)))
               (is (= "yes" (aget (.-headers res) "x-fixture")))
               (is (= "/query?f=json" (.-url body)))
               (is (= "GET" (.-method body)))
               (is (= "client" (aget (.-headers body) "x-orig")))
               (is (= "bridge-decorator" (aget (.-headers body) "x-injected"))))))))

(deftest ^:async installXhrPolyfill-installs-a-global-synchronous-XMLHttpRequest
  (let [saved-xhr (.-XMLHttpRequest js/globalThis)]
    (try
      (await (with-echo-server
               (fn ^:async xhr-round-trip [base]
                 (await (installXhrPolyfill
                         #js {:syncFetch (await (createSyncFetch #js {:workerUrl worker-url}))}))
                 (let [xhr (new js/globalThis.XMLHttpRequest)]
                   (.open xhr "GET" (str base "/xhr") false)
                   (.setRequestHeader xhr "x-from-xhr" "1")
                   (.send xhr)
                   (is (= 200 (.-status xhr)))
                   (is (thrown? js/Error (.open (new js/globalThis.XMLHttpRequest) "GET" "/" true)))
                   (let [body (js/JSON.parse (decode (js/Uint8Array. (.-response xhr))))]
                     (is (= "/xhr" (.-url body)))
                     (is (= "1" (aget (.-headers body) "x-from-xhr"))))))))
      (finally
        (set! (.-XMLHttpRequest js/globalThis) saved-xhr)))))

(deftest ^:async shutdown-with-no-worker-returns-false
  (is (false? (await (shutdown)))))

(deftest ^:async shutdown-is-reference-counted-across-consumers
  ;; Two libraries in one worker thread share one fetch worker.
  (await (with-echo-server
           (fn ^:async ref-counted [base]
             (let [fetch-a (await (createSyncFetch #js {:workerUrl worker-url}))
                   fetch-b (await (createSyncFetch #js {:workerUrl worker-url}))
                   plain (str base "/plain")]
               (is (= 200 (.-status (fetch-a plain))))
               (is (false? (await (shutdown))) "the first release keeps the shared worker up")
               (is (= 200 (.-status (fetch-b plain))) "B survives the release of A")
               (is (true? (await (shutdown))) "the last release ends the worker")
               (let [fetch-c (await (createSyncFetch #js {:workerUrl worker-url}))]
                 (is (= 200 (.-status (fetch-c plain))) "a later consumer gets a new worker")))))))

(deftest ^:async a-decorator-that-fails-to-import-rejects
  ;; A worker that serves after a failed decorator import drops auth. The
  ;; message rules out the readiness timeout.
  (is (.includes (await (create-error-message #js {:workerUrl worker-url
                                                   :decorateUrl broken-decorate-url}))
                 "failed to initialize"))
  (is (.includes (await (create-error-message #js {:workerUrl worker-url
                                                   :decorateUrl noexport-decorate-url}))
                 "failed to initialize")))

(deftest ^:async a-hanging-decorator-times-out-to-status-0
  ;; The decorator hangs on the first request only. Without the abort at
  ;; requestTimeoutMs, the worker stays on that request and the second gives 0.
  (await (with-echo-server
           (fn ^:async hang [base]
             (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                           :decorateUrl hang-decorate-url
                                                           :requestTimeoutMs 100}))]
               (is (= 0 (.-status (sync-fetch (str base "/plain")))))
               (is (= 200 (.-status (sync-fetch (str base "/plain"))))))))))

(deftest ^:async follows-a-multi-hop-redirect-chain
  ;; /redirect-chain -> /redirect-chain-2 -> /query?redirected=2
  (await (with-echo-server
           (fn ^:async redirects [base]
             (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url}))
                   res (sync-fetch (str base "/redirect-chain"))]
               (is (= 200 (.-status res)))
               (is (= "/query?redirected=2" (.-url (echoed res)))))))))

(deftest ^:async a-slow-but-steady-download-is-not-killed-by-the-request-timeout
  ;; Each chunk restarts the idle timer. The 400 ms transfer must outlast
  ;; requestTimeoutMs, or the test proves nothing.
  (await (with-echo-server
           (fn ^:async steady [base]
             (let [sync-fetch (await (createSyncFetch #js {:workerUrl worker-url
                                                           :requestTimeoutMs 300}))
                   res (sync-fetch (str base "/slow?chunks=4&delay=100"))]
               (is (= 200 (.-status res)))
               (is (= 40 (.-length (.-bodyBytes res)))))))))

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

(deftest ^:async with-no-workerUrl-node-starts-the-fetch-worker-next-to-the-bridge
  (await (with-echo-server
           (fn ^:async default-worker [base]
             (let [sync-fetch (await (createSyncFetch))]
               (is (= "/default" (.-url (echoed (sync-fetch (str base "/default") #js {}))))))))))

(tr/run-tests-and-exit! "net.willcohen.native.http-bridge-test")
