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
                     get_context_worker
                     evict_oldest_BANG_
                     bounded_create_handle_BANG_
                     evicted_QMARK_
                     ref_handle_BANG_
                     unref_handle_BANG_
                     get_pool_stats]]
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

(deftest expose-gc-is-set
  (is (= "function" (js* "typeof globalThis.gc"))
      "this suite requires `node --expose-gc`"))

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

(deftest reset-library-context-drains-every-claim
  (let [lib "pool-test-reset"]
    (register_library_context_BANG_ lib)
    (let [releases (atom 0)
          owners #js [#js {} #js {} #js {}]]
      (doseq [i (range (.-length owners))]
        (track_context_BANG_ lib i i
                             (fn [] (swap! releases inc))
                             (aget owners i)))
      (reset_library_context_BANG_ lib)
      (is (= (.-length owners) @releases)))))

(deftest worker-idx-from-args-resolves-affinity-and-falls-back
  (let [lib "affinity-test"]
    (register_library_context_BANG_ lib)
    ;; .worker_idx is the munged :worker-idx a consumer tags on its context.
    (is (= 3 (worker_idx_from_args lib #js [#js {:worker_idx 3} "scalar"])))
    ;; The fallback is a fixed 0, since dispatch cannot tell a pure call from
    ;; one that touches worker-local module state.
    (is (= 0 (worker_idx_from_args lib #js ["scalar" 42])))
    (reset_library_context_BANG_ lib)))

(deftest assign-worker-for-context-honors-explicit-worker-and-bounds-checks
  (let [lib "assign-test"
        fake-pool #js {:size 4}]
    (register_library_context_BANG_ lib)
    (let [assigned (assign_worker_for_context_BANG_ fake-pool lib {:worker 2})]
      (is (= 2 (:idx assigned)) "explicit :worker is honored without a claim")
      (is (fn? (:release assigned)) "a release closure is always returned"))
    (is (thrown? js/Error
                 (assign_worker_for_context_BANG_ fake-pool lib {:worker 9}))
        "out-of-range worker index is rejected")
    (reset_library_context_BANG_ lib)))

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
      (track_context_BANG_ libB 1 3 (fn [] (swap! relB inc)) owner-b)
      (is (= 2 (get_context_worker libA 1)) "libA ctx 1 pinned to worker 2")
      (is (= 3 (get_context_worker libB 1)) "libB ctx 1 pinned to worker 3")
      (reset_library_context_BANG_ libA)
      (is (= 1 @relA) "libA reset fired libA's release")
      (is (= 0 @relB) "libA reset left libB untouched")
      (is (= 3 (get_context_worker libB 1)) "libB context survives libA reset")
      (reset_library_context_BANG_ libB)
      (is (= 1 @relB))
      ;; Keep owners reachable so no FR fires mid-test and perturbs counts.
      (is (= "a" (.-k owner-a)))
      (is (= "b" (.-k owner-b))))))

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
    (register_library_context_BANG_ lib #js {:max_live_ctxs 3 :min_age_ms 0})
    (register_handle_BANG_ lib "c0" 0 (mk "c0") o0)
    (register_handle_BANG_ lib "c1" 0 (mk "c1") o1)
    (register_handle_BANG_ lib "c2" 0 (mk "c2") o2)
    (is (= "evicted" (evict_oldest_BANG_ lib))
        "idle entry is evictable even though its owner is still reachable")
    (is (= 1 (.-length log)) "exactly one release fired")
    (let [victim (aget log 0)]
      (is (true? (evicted_QMARK_ lib victim)) "evicted ctx-id is tombstoned")
      (is (= 1 (count (filter #(= true (aget % "__cljNativeEvicted")) [o0 o1 o2])))
          "exactly one owner marked invalid"))
    (is (= 2 (.-live (get_pool_stats lib))) "live dropped by one")
    (reset_library_context_BANG_ lib)))

(deftest bounded-create-caps-live-count-by-evicting
  (let [lib "evict-bound"
        owners #js []]
    (register_library_context_BANG_ lib #js {:max_live_ctxs 4 :min_age_ms 0})
    (dotimes [i 10]
      (let [o #js {:id i}]
        (.push owners o)
        (bounded_create_handle_BANG_
         lib (fn [] (register_handle_BANG_ lib (str "b" i) 0 (fn [] nil) o)))))
    (let [stats (get_pool_stats lib)]
      (is (= 4 (.-live stats)) "live count parks at the bound via eviction")
      (is (= 6 (.-evictions stats)) "each create beyond the bound evicted one")
      (is (= 0 (.-blocks stats)) "no blocks: eviction always found a victim"))
    (is (= 10 (.-length owners)))                     ; all owners still reachable
    (reset_library_context_BANG_ lib)))

(deftest refcount-still-pins-against-eviction
  (let [lib "evict-refcount"
        o #js {}]
    (register_library_context_BANG_ lib #js {:max_live_ctxs 2 :min_age_ms 0})
    (register_handle_BANG_ lib "p" 0 (fn [] nil) o)
    (ref_handle_BANG_ lib "p")
    (is (= "none-evictable" (evict_oldest_BANG_ lib)) "refcount>0 pins the entry")
    (unref_handle_BANG_ lib "p")
    (is (= "evicted" (evict_oldest_BANG_ lib)) "unref makes it evictable")
    (is (identical? o o))                             ; keep o alive
    (reset_library_context_BANG_ lib)))

(deftest reregister-clears-eviction-tombstone
  (let [lib "evict-reregister"]
    (register_library_context_BANG_ lib #js {:max_live_ctxs 1 :min_age_ms 0})
    (register_handle_BANG_ lib "x" 0 (fn [] nil) #js {})
    (evict_oldest_BANG_ lib)
    (is (true? (evicted_QMARK_ lib "x")) "evicted ctx-id is tombstoned")
    (register_handle_BANG_ lib "x" 0 (fn [] nil) #js {})
    (is (false? (evicted_QMARK_ lib "x")) "fresh registration clears the tombstone")
    (reset_library_context_BANG_ lib)))

(tr/run-tests-and-exit! "net.willcohen.native.pool-test")
