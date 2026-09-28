;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.pool-test
  (:require [cljs.test :refer [deftest is]]
            ["ffi-wasm/pool"
             :refer [register_library_context_BANG_
                     track_context_BANG_
                     untrack_context_BANG_
                     reset_library_context_BANG_
                     register_handle_BANG_
                     dispose_handle_BANG_
                     in_flight_count_for_parent
                     await_parent_drain_BANG_
                     worker_idx_from_args
                     assign_worker_for_context_BANG_
                     evict_oldest_BANG_
                     bounded_create_handle_BANG_
                     evicted_QMARK_
                     ref_handle_BANG_
                     unref_handle_BANG_
                     get_pool_detail
                     get_pool_stats
                     fire_and_capture_dispose_BANG_
                     flush_pending_disposes_BANG_
                     set_log_config_BANG_]]
            ["ffi-wasm/handler-runtime" :refer [isEnabled]]
            ["ffi-wasm/test-runner" :as tr]))

(defn ^:async gc-until
  "Run a major GC and yield, until (done?) or 20 rounds."
  [done?]
  (loop [i 0]
    (when (and (< i 20) (not (done?)))
      (.gc js/globalThis)
      (await (js/Promise. (fn [resolve _reject] (js/setImmediate resolve))))
      (recur (inc i)))))

;; Reachable for the whole run, so its release must never fire on GC.
(def ^:private live-owner #js {:kind "ctx-2"})

;; Allocate the owner here, so no caller stack frame pins it across the gc() loop.
(defn track-ephemeral [lib ctx-id worker-idx release-fn]
  (let [owner #js {:kind ctx-id}]
    (track_context_BANG_ lib ctx-id worker-idx release-fn owner)))

(deftest ^:async fr-driven-release-fires-for-gc-collected-owners-and-skips-live-ones
  (let [lib "pool-test-sweep"]
    (register_library_context_BANG_ lib)
    (let [release1 (atom 0)
          release2 (atom 0)
          release1-fn (fn [] (swap! release1 inc))
          release2-fn (fn [] (swap! release2 inc))]
      (track-ephemeral lib 1 0 release1-fn)
      (track_context_BANG_ lib 2 1 release2-fn live-owner)
      (await (gc-until #(pos? @release1)))
      (is (= 1 @release1) "release1-fn fired exactly once on FR drain")
      (is (= 0 @release2) "release2-fn did not fire (live-owner still alive)")
      (reset_library_context_BANG_ lib))))

(deftest untrack-context-fires-release-for-tracked-entries
  (let [lib "pool-test-untrack"]
    (register_library_context_BANG_ lib)
    (let [releases (atom 0)
          owner #js {:kind "only"}]
      (track_context_BANG_ lib "only" 0 (fn [] (swap! releases inc)) owner)
      (untrack_context_BANG_ lib "only")
      (is (= 1 @releases) "untrack fires release once")
      (untrack_context_BANG_ lib "only")
      (is (= 1 @releases))
      (reset_library_context_BANG_ lib))))

(deftest worker-idx-from-args-resolves-affinity-and-falls-back
  ;; .worker_idx is the munged :worker-idx a consumer tags on its context.
  (is (= 3 (worker_idx_from_args #js [#js {:worker_idx 3} "scalar"])))
  (is (= 0 (worker_idx_from_args #js ["scalar" 42]))))

(deftest assign-worker-for-context-honors-explicit-worker-and-bounds-checks
  (let [lib "assign-test"
        fake-pool #js {:size 4}]
    (is (= 2 (:idx (assign_worker_for_context_BANG_ fake-pool lib {:worker 2})))
        "explicit :worker is honored without a claim")
    (is (thrown? js/Error
                 (assign_worker_for_context_BANG_ fake-pool lib {:worker 9}))
        "out-of-range worker index is rejected")
    (is (thrown? js/Error
                 (assign_worker_for_context_BANG_ fake-pool lib {:worker -1}))
        "a negative worker index is rejected")))

(deftest two-library-contexts-stay-isolated
  ;; Two handler-keys that share one pool keep independent context registries.
  (let [libA "iso-A"
        libB "iso-B"]
    (register_library_context_BANG_ libA)
    (register_library_context_BANG_ libB)
    (let [relA (atom 0)
          relB (atom 0)
          owner-a #js {:k "a"}
          owner-b #js {:k "b"}]
      (track_context_BANG_ libA 1 2 (fn [] (swap! relA inc)) owner-a)
      (track_context_BANG_ libA 2 2 (fn [] (swap! relA inc)) #js {})
      (track_context_BANG_ libB 1 3 (fn [] (swap! relB inc)) owner-b)
      (reset_library_context_BANG_ libA)
      (is (= 2 @relA) "libA reset fired every libA release")
      (is (= 0 @relB) "libA reset left libB untouched")
      (reset_library_context_BANG_ libB)
      (is (= 1 @relB)))))

(deftest ^:async parent-drain-gate-stays-open-until-child-release-promises-settle
  ;; A parent must count its children drained only when each async release-fn
  ;; Promise settles, not when the child leaves the live map.
  (let [lib "drain-test"
        parent 100]
    (register_library_context_BANG_ lib)
    (let [resolvers (atom [])
          pending-release (fn []
                            (js/Promise. (fn [res _reject]
                                           (swap! resolvers conj res))))
          owner-1 #js {:k 1}
          owner-2 #js {:k 2}]
      ;; Pass the fn itself, so each dispose mints a fresh pending Promise.
      (register_handle_BANG_ lib 1 0 pending-release owner-1 parent)
      (register_handle_BANG_ lib 2 0 pending-release owner-2 parent)
      (is (= 2 (in_flight_count_for_parent parent)) "both children counted")
      (dispose_handle_BANG_ lib 1)
      (dispose_handle_BANG_ lib 2)
      (is (= 2 (count @resolvers)) "both release-fns were invoked")
      (is (= 2 (in_flight_count_for_parent parent))
          "gate stays open while release Promises are unsettled (closes the TOCTOU window)")
      (let [drained (atom false)
            drain-p (.then (await_parent_drain_BANG_ parent)
                           (fn [_] (reset! drained true)))]
        (doseq [r @resolvers] (r nil))
        (await drain-p)
        (is (= 0 (in_flight_count_for_parent parent)) "gate closes after both settle")
        (is (true? @drained) "await-parent-drain! resolved only after all releases settled"))
      (reset_library_context_BANG_ lib))))

;; Owners stay reachable in the eviction tests, so an owner-alive gate on
;; eviction would make them fail.

(deftest eviction-reclaims-idle-entry-with-owner-still-reachable
  (let [lib "evict-reclaim"
        o0 #js {:id 0} o1 #js {:id 1} o2 #js {:id 2}
        log #js []
        mk (fn [id] (fn [] (.push log id)))]
    (register_library_context_BANG_ lib {:max-live-ctxs 3 :min-age-ms 0})
    (register_handle_BANG_ lib 0 0 (mk 0) o0)
    (register_handle_BANG_ lib 1 0 (mk 1) o1)
    (register_handle_BANG_ lib 2 0 (mk 2) o2)
    (is (= "evicted" (evict_oldest_BANG_ lib))
        "idle entry is evictable even though its owner is still reachable")
    (is (= 1 (.-length log)) "exactly one release fired")
    (is (= 2 (.-live (get_pool_stats lib))) "live dropped by one")
    (let [victim (aget log 0)]
      ;; A numeric id: the live map keys it as a string, the tombstone must not.
      (is (true? (evicted_QMARK_ lib victim)) "evicted ctx-id is tombstoned")
      (register_handle_BANG_ lib victim 0 (mk victim) #js {})
      (is (false? (evicted_QMARK_ lib victim)) "a fresh registration clears the tombstone"))
    (reset_library_context_BANG_ lib)))

(deftest bounded-create-caps-live-count-by-evicting
  (let [lib "evict-bound"]
    (register_library_context_BANG_ lib {:max-live-ctxs 4 :min-age-ms 0})
    (dotimes [i 10]
      (let [o #js {:id i}]
        (bounded_create_handle_BANG_
         lib (fn [] (register_handle_BANG_ lib (str "b" i) 0 (fn [] nil) o)))))
    (let [stats (get_pool_stats lib)]
      (is (= 4 (.-live stats)) "live count parks at the bound via eviction")
      (is (= 6 (.-evictions stats)) "each create beyond the bound evicted one")
      (is (= 0 (.-blocks stats)) "no blocks: eviction always found a victim"))
    (reset_library_context_BANG_ lib)))

(deftest refcount-still-pins-against-eviction
  (let [lib "evict-refcount"
        o #js {}]
    (register_library_context_BANG_ lib {:max-live-ctxs 2 :min-age-ms 0})
    (register_handle_BANG_ lib "p" 0 (fn [] nil) o)
    (ref_handle_BANG_ lib "p")
    (let [detail (get_pool_detail lib)
          age-ms (.-age_ms (aget (.-sample detail) 0))]
      (is (= {:total 1 :evictable 0 :blocked_refcount 1 :blocked_age_gate 0
              :sample [{:ctx_id "p" :refcount 1 :owner_alive true
                        :age_ms age-ms :age_gated false}]}
             detail)
          "get-pool-detail keeps the JS shape that clj-proj re-exports"))
    (is (= "none-evictable" (evict_oldest_BANG_ lib)) "refcount>0 pins the entry")
    (unref_handle_BANG_ lib "p")
    (is (= "evicted" (evict_oldest_BANG_ lib)) "unref makes it evictable")
    (reset_library_context_BANG_ lib)))

(deftest ^:async a-flush-holds-only-pending-and-failed-disposes
  ;; A page flushes only at shutdown, so a fulfilled dispose must leave the list.
  (await (flush_pending_disposes_BANG_))
  (dotimes [_ 3]
    (fire_and_capture_dispose_BANG_ (fn [] (js/Promise.resolve nil)) nil))
  (fire_and_capture_dispose_BANG_ (fn [] (js/Promise.reject (js/Error. "destroy failed"))) nil)
  (await (js/Promise. (fn [resolve _reject] (js/setImmediate resolve))))
  (let [settled (await (flush_pending_disposes_BANG_))]
    (is (= 1 (.-length settled)))
    (is (= "rejected" (.-status (aget settled 0))))))

(deftest set-log-config!-takes-a-lazy-seq-of-categories
  (set_log_config_BANG_ {:level :debug :categories (map identity [:busy])})
  (is (isEnabled "BUSY-INC"))
  (set_log_config_BANG_ nil))

(tr/run-tests-and-exit! "net.willcohen.native.pool-test")
