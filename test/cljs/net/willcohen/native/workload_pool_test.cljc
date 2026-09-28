;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.workload-pool-test
  (:require [cljs.test :refer [deftest is testing]]
            ["ffi-wasm/workload-pool" :as wp]
            ["ffi-wasm/pool" :as pool]
            ["ffi-wasm/test-runner" :as tr]
            ["node:url" :refer [fileURLToPath pathToFileURL]]
            ["node:path" :refer [join dirname]]))

(def this-dir (dirname (fileURLToPath (.-url js/import.meta))))
;; Four levels up from test/cljs/net/willcohen/native is test/.
(def test-dir (join this-dir ".." ".." ".." ".."))
(def handler-url
  (.-href (pathToFileURL (join test-dir "fixtures" "registry-handler.mjs"))))

(defn- resolved-fake-pool
  "Pool-shaped object whose terminate records into `calls` and resolves. As
   in worker-router, terminate needs no `this`."
  [calls]
  #js {:terminate (fn terminate []
                    (swap! calls conj :terminate)
                    (js/Promise.resolve nil))})

(deftest init-workload-pool!-returns-a-single-pool-registry
  (let [reg (wp/init-workload-pool! {:size 2})]
    (is (= :cljs (:runtime reg)))
    (is (= false @(:terminated? reg)))
    (is (= {:size 2} (:opts reg)) "opts passed through verbatim")
    (testing "single-pool state, empty until ensure-pool!/adopt-pool!"
      (is (= [] @(:handlers reg)))
      (is (nil? @(:pool reg)))
      (is (= false @(:owned? reg)))
      (is (nil? @(:latch reg)))
      (is (= 0 @(:generation reg))))))

(deftest register-handler!-ignores-workload-and-accumulates-in-order
  (let [reg (wp/init-workload-pool! {})
        ret (wp/register-handler! reg :compute :lib-a {:module "lib-a.mjs"})]
    (is (identical? reg ret) "returns the same registry for threading")
    (wp/register-handler! reg :io :lib-b {:module "lib-b.mjs"})
    (wp/register-handler! reg :mixed :hook-only {:pre-terminate (fn [] nil)})
    (let [entries @(:handlers reg)]
      (is (= 3 (count entries))
          "different workload args land in the ONE flat vector")
      (is (= [:lib-a :lib-b :hook-only] (mapv :lib-key entries))
          "registration order preserved")
      (is (= "lib-a.mjs" (:module (first entries)))))))

(deftest register-handler!-replaces-a-re-registered-lib-key-in-place
  (let [reg (wp/init-workload-pool! {})]
    (wp/register-handler! reg :compute :lib-a {:module "a1.mjs"})
    (wp/register-handler! reg :compute :lib-b {:module "b.mjs"})
    (wp/register-handler! reg :compute :lib-a {:module "a2.mjs"})
    (let [entries @(:handlers reg)]
      (is (= [:lib-a :lib-b] (mapv :lib-key entries))
          "replacement keeps the original position in the shutdown walk")
      (is (= "a2.mjs" (:module (first entries))) "spec replaced"))))

(deftest register-handler!-rejects-a-spec-with-neither-module-nor-pre-terminate
  (let [reg (wp/init-workload-pool! {})]
    (is (thrown-with-msg? js/Error #":module or :pre-terminate"
                          (wp/register-handler! reg :compute :lib-a
                                                {:args #js {}})))))

(deftest register-handler!-rejects-a-module-spec-once-a-pool-exists
  (let [reg (wp/init-workload-pool! {})]
    (wp/adopt-pool! reg (resolved-fake-pool (atom [])))
    (is (thrown-with-msg? js/Error #"before ensure-pool!"
                          (wp/register-handler! reg :compute :late
                                                {:module "late.mjs"})))
    (is (identical? reg (wp/register-handler! reg :compute :hook-only
                                              {:pre-terminate (fn [] nil)}))
        "a :pre-terminate-only spec may still register after adoption")))

(deftest current-context-throws-on-cljs-reserved-for-the-worker-side
  ;; Per-worker state lives in Web Workers, out of synchronous reach.
  (is (thrown-with-msg? js/Error #"not callable from the main thread"
                        (wp/current-context :lib-a))))

(deftest ^:async adopt-pool!-stores-an-external-pool-unowned
  (let [reg (wp/init-workload-pool! {})
        calls (atom [])
        fake (resolved-fake-pool calls)
        ret (wp/adopt-pool! reg fake)]
    (is (identical? reg ret) "returns the registry")
    (is (identical? fake @(:pool reg)))
    (is (= false @(:owned? reg)) "adopted, not owned")
    (is (identical? fake (await (wp/ensure-pool! reg)))
        "ensure-pool! resolves to the adopted pool")))

(deftest current-pool-reads-the-live-pool-through-the-registry
  ;; A consumer bundled with its own squint-cljs copy cannot deref this
  ;; package's Atoms, since protocol symbols are per instance.
  (let [reg (wp/init-workload-pool! {})]
    (is (nil? (wp/current-pool reg)) "nil before adopt-pool!/ensure-pool!")
    (let [fake (resolved-fake-pool (atom []))]
      (wp/adopt-pool! reg fake)
      (is (identical? fake (wp/current-pool reg))
          "derefs :pool in the registry's home squint instance"))))

(deftest adopt-pool!-throws-when-a-pool-is-already-present
  (let [reg (wp/init-workload-pool! {})]
    (wp/adopt-pool! reg (resolved-fake-pool (atom [])))
    (is (thrown-with-msg? js/Error #"already present"
                          (wp/adopt-pool! reg (resolved-fake-pool (atom [])))))))

(deftest ^:async ensure-pool!-rejects-without-a-module-spec-and-clears-the-latch
  (let [reg (wp/init-workload-pool! {})]
    (wp/register-handler! reg :compute :hook-only {:pre-terminate (fn [] nil)})
    (let [err (await (-> (wp/ensure-pool! reg)
                         (.then (fn [_] nil))
                         (.catch (fn [e] e))))]
      (is (some? err) "rejects: no registered spec carries a :module")
      (is (.includes (.-message err) "carries a :module"))
      (is (nil? @(:latch reg)) "latch cleared so a later call can retry"))))

(deftest ^:async ensure-pool!-folds-every-spec-into-one-real-pool-and-shutdown-recycles-it
  (let [reg (wp/init-workload-pool! {:size 1})
        hook-calls (atom [])]
    (wp/register-handler! reg :compute :lib-a
                          {:module handler-url
                           :args #js {:tag "a"}
                           :pre-terminate (fn [] (swap! hook-calls conj :a) nil)})
    (wp/register-handler! reg :io :lib-b
                          {:module handler-url
                           :args #js {:tag "b"}
                           :pre-terminate (fn []
                                            (swap! hook-calls conj :b)
                                            (js/Promise.resolve nil))})
    (let [p1 (wp/ensure-pool! reg)
          p2 (wp/ensure-pool! reg)]
      (is (identical? p1 p2) "latched: both callers share the same init promise")
      (let [pl (await p1)]
        (is (identical? pl @(:pool reg)))
        (is (= true @(:owned? reg)) "spawned here, so owned")
        (is (= 1 (pool/pool-size pl)) "ONE joint pool, sized as configured")
        (is (= "pong:a" (await (pool/worker-call pl :lib-a "ping" #js [] 0)))
            "lib-a answers with its own :args payload")
        (is (= "pong:b" (await (pool/worker-call pl :lib-b "ping" #js [] 0)))
            "lib-b answers on the SAME worker with its own payload")
        (let [ret (await (wp/shutdown-pool! reg))]
          (is (identical? reg ret) "shutdown resolves to the registry")
          (is (= [:b :a] @hook-calls)
              "pre-terminate hooks ran in reverse registration order")
          (is (= true @(:terminated? reg)))
          (is (nil? @(:pool reg)))
          (is (nil? @(:latch reg)))
          (is (= false @(:owned? reg)))
          (is (= 1 @(:generation reg)) "generation bumped"))
        (is (= :rejected (await (-> (pool/worker-call pl :lib-a "ping" #js [] 0)
                                    (.then (fn [_] :resolved))
                                    (.catch (fn [_] :rejected)))))
            "the owned pool was really terminated")
        (is (nil? (await (wp/shutdown-pool! reg)))
            "idempotent: a second shutdown is a no-op")))))

(deftest ^:async shutdown-pool!-leaves-an-adopted-pool-up-and-still-walks-hooks
  (let [reg (wp/init-workload-pool! {})
        calls (atom [])
        hook-calls (atom [])
        fake (resolved-fake-pool calls)]
    (wp/register-handler! reg :compute :hook-only
                          {:pre-terminate (fn [] (swap! hook-calls conj :hook) nil)})
    (wp/adopt-pool! reg fake)
    (let [ret (await (wp/shutdown-pool! reg))]
      (is (identical? reg ret))
      (is (= 0 (count @calls)) "adopted pool NOT terminated; its creator owns it")
      (is (= [:hook] @hook-calls) "hooks still ran")
      (is (= true @(:terminated? reg)))
      (is (= 1 @(:generation reg)) "generation bumped"))))

(deftest ^:async shutdown-pool!-survives-a-rejecting-pre-terminate-hook
  (let [reg (wp/init-workload-pool! {})
        hook-calls (atom [])]
    (wp/register-handler! reg :compute :lib-a
                          {:pre-terminate (fn [] (swap! hook-calls conj :a) nil)})
    (wp/register-handler! reg :compute :lib-b
                          {:pre-terminate (fn []
                                            (swap! hook-calls conj :b)
                                            (js/Promise.reject (js/Error. "boom")))})
    (wp/adopt-pool! reg (resolved-fake-pool (atom [])))
    (let [ret (await (wp/shutdown-pool! reg))]
      (is (identical? reg ret) "a rejecting hook still resolves to the registry")
      (is (= [:b :a] @hook-calls)
          "the walk continued past the rejection, still in reverse order")
      (is (= true @(:terminated? reg))))))

(deftest ^:async shutdown-pool!-survives-a-terminate-that-rejects
  ;; Only ensure-pool! makes an owned pool, so inject owned state through the
  ;; documented registry shape.
  (let [reg (wp/init-workload-pool! {})
        fake #js {:terminate (fn terminate []
                               (js/Promise.reject (js/Error. "terminate failed")))}]
    (reset! (:pool reg) fake)
    (reset! (:owned? reg) true)
    (reset! (:latch reg) (js/Promise.resolve fake))
    (let [ret (await (wp/shutdown-pool! reg))]
      (is (identical? reg ret) "a rejecting terminate still resolves to the registry")
      (is (= true @(:terminated? reg)))
      (is (nil? @(:pool reg)) "cleanup completed"))))

(deftest make-wiring!-starts-with-an-empty-registry-and-memo
  (let [wiring (wp/make-wiring!)]
    (is (nil? @(:registry wiring)) "no registry until a pass runs")
    (is (nil? @(:latch wiring)) "no memo until a pass runs")
    ;; squint's nil? also passes for the undefined a tail `when` returns.
    ;; identical? compiles to ===.
    (is (identical? nil (wp/wiring-pool wiring))
        "no pool before ensure-wired!, and null rather than undefined")))

(deftest ^:async ensure-wired!-latches-the-whole-pass-so-one-registry-is-built
  ;; The setup builds the registry, so the registry cannot guard it. Without
  ;; the wiring, each caller spawns a pool.
  (let [wiring    (wp/make-wiring!)
        built     (atom [])
        register! (fn ^:async register! [reg]
                    ;; A real consumer awaits its init payload here: the race window.
                    (await (js/Promise.resolve nil))
                    (swap! built conj reg)
                    (wp/register-handler! reg :compute :lib-a
                                          {:module handler-url
                                           :args #js {:tag "a"}}))
        opts      {:registry-opts {:size 1} :register! register!}
        p1        (wp/ensure-wired! wiring opts)
        p2        (wp/ensure-wired! wiring opts)]
    (is (identical? p1 p2) "concurrent callers share one pass promise")
    (let [pl (await p1)]
      (is (= 1 (count @built)) "ONE registry built, so ONE pool spawned")
      (is (identical? pl (wp/wiring-pool wiring)))
      (is (= 1 (pool/pool-size pl)) "the pass honored :registry-opts")
      (is (= "pong:a" (await (pool/worker-call pl :lib-a "ping" #js [] 0)))
          "the registered spec really reached the worker")
      (is (identical? pl (await (wp/ensure-wired! wiring opts)))
          "a later call yields the first pass's pool")
      (await (wp/shutdown-wiring! wiring)))))

(deftest ^:async ensure-wired!-adopts-a-caller-pool-and-shutdown-leaves-it-up
  (let [wiring (wp/make-wiring!)
        calls  (atom [])
        hooks  (atom [])
        fake   (resolved-fake-pool calls)
        pl     (await (wp/ensure-wired!
                       wiring
                       {:pool fake
                        :register!
                        (fn [reg]
                          (wp/register-handler!
                           reg :compute :hook-only
                           {:pre-terminate (fn [] (swap! hooks conj :hook) nil)}))}))]
    (is (identical? fake pl) "resolves to the caller's pool, nothing spawned")
    (is (identical? fake (wp/wiring-pool wiring)))
    (let [reg (await (wp/shutdown-wiring! wiring))]
      (is (some? reg) "resolves to the registry it tore down")
      (is (= false @(:owned? reg)) "adopted, so never owned")
      (is (= [:hook] @hooks) "the pre-terminate walk still ran")
      (is (= 0 (count @calls)) "the caller's pool was left up")
      (is (nil? @(:registry wiring)) "wiring cleared")
      (is (nil? @(:latch wiring)) "memo cleared, so a later pass is fresh")
      (is (identical? nil (wp/wiring-pool wiring))
          "null rather than undefined after shutdown too"))))

(deftest ^:async shutdown-wiring!-yields-nil-when-nothing-was-wired
  (let [wiring (wp/make-wiring!)]
    (is (nil? (await (wp/shutdown-wiring! wiring)))
        "nil tells a consumer to run its own never-wired cleanup")))

(deftest ^:async ensure-wired!-clears-the-wiring-when-the-pass-rejects
  (let [wiring    (wp/make-wiring!)
        attempts  (atom 0)
        fake      (resolved-fake-pool (atom []))
        register! (fn ^:async register! [reg]
                    (swap! attempts inc)
                    (when (= 1 @attempts)
                      (throw (js/Error. "register boom")))
                    (wp/register-handler! reg :compute :hook-only
                                          {:pre-terminate (fn [] nil)}))
        opts      {:pool fake :register! register!}
        err       (await (-> (wp/ensure-wired! wiring opts)
                             (.then (fn [_] nil))
                             (.catch (fn [e] e))))]
    (is (some? err) "the pass rejects with the consumer's error")
    (is (nil? @(:latch wiring)) "memo cleared so a later call retries")
    (is (nil? @(:registry wiring)) "the half-built registry is gone")
    (let [pl (await (wp/ensure-wired! wiring opts))]
      (is (= 2 @attempts) "the retry ran a fresh pass")
      (is (identical? fake pl))
      (await (wp/shutdown-wiring! wiring)))))

(deftest ^:async live-pool?-keys-on-pool-identity-not-on-a-generation-counter
  (let [wiring (wp/make-wiring!)
        fake   (resolved-fake-pool (atom []))
        other  (resolved-fake-pool (atom []))]
    (is (= false (wp/live-pool? wiring fake)) "nothing is live before a pass")
    (await (wp/ensure-wired! wiring {:pool fake}))
    (is (wp/live-pool? wiring fake) "the wired pool is live")
    (is (= false (wp/live-pool? wiring other)) "another pool is not")
    (is (= false (wp/live-pool? wiring nil)) "nil is never live")
    (await (wp/shutdown-wiring! wiring))
    (is (= false (wp/live-pool? wiring fake)) "no pool, so nothing is live")
    ;; An adopted pool keeps its workers across shutdown, so its handles'
    ;; teardowns must fire after re-adoption. A check on :generation would
    ;; drop them and leak their native memory.
    (await (wp/ensure-wired! wiring {:pool fake}))
    (is (wp/live-pool? wiring fake) "the same pool object is live again")
    (await (wp/shutdown-wiring! wiring))))

(tr/run-tests-and-exit! "net.willcohen.native.workload-pool-test")
