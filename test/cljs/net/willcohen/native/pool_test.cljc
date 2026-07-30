;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
;;
;; cljs.test suite for LibraryContext lifetime in pool.cljc. Uses
;; squint's cljs.test adapter (node_modules/squint-cljs/src/squint/test.js):
;; (deftest ^:async name body) returns a Promise that test_var awaits.
;;
;; The cljs branch delegates to resource-tracker (FinalizationRegistry
;; under the hood), so the disposefn fires once V8 collects the JS owner.
;; Requires --expose-gc plus an event-loop yield (setImmediate) so the FR
;; callback drains before the next assertion.

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

;; Five major-GC cycles with an async yield between each is what V8
;; empirically needs to clear a fresh WeakRef on darwin/arm64 Node 22.
;; One cycle frees the object; subsequent cycles let the WeakRef
;; bookkeeping observe and clear deref() to undefined.
(defn ^:async flush-gc []
  (loop [i 0]
    (when (< i 5)
      (.gc js/globalThis)
      (await (js/Promise. (fn [resolve _reject] (js/setImmediate resolve))))
      (recur (inc i)))))

;; Allocate + register the owner inside a helper so the strong reference
;; goes out of scope on return. If the test held `owner` as a local,
;; V8's stack-frame retention can pin the object across our gc() loop
;; and starve the WeakRef.
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
          release2-fn (fn [] (swap! release2 inc))
          owner2 #js {:kind "ctx-2"}]
      ;; owner1 is allocated inside a helper so the only reference is the
      ;; WeakRef inside the library's ctx-workers map; owner2 stays
      ;; strongly referenced from this scope.
      (track-ephemeral lib 1 0 release1-fn)
      (track_context_BANG_ lib 2 1 release2-fn owner2)
      (is (= 0 @release1))
      (is (= 0 @release2))
      (await (flush-gc))
      ;; After GC + event-loop yields, resource-tracker's
      ;; FinalizationRegistry callback has fired for owner1 (collected)
      ;; but not owner2 (still strongly held).
      (is (= 1 @release1) "release1-fn fired exactly once on FR drain")
      (is (= 0 @release2) "release2-fn did not fire (owner2 still alive)")
      (reset_library_context_BANG_ lib)
      ;; Touch owner2 here so the JIT cannot dead-store its assignment
      ;; and collect it before the sweep above runs.
      (is (= "ctx-2" (.-kind owner2))))))

(deftest untrack-context-fires-release-for-tracked-entries
  (let [lib "pool-test-untrack"]
    (register_library_context_BANG_ lib)
    (let [releases (atom 0)
          owner #js {:kind "only"}]
      (track_context_BANG_ lib "only" 0 (fn [] (swap! releases inc)) owner)
      (untrack_context_BANG_ lib "only")
      (is (= 1 @releases) "untrack fires release once")
      ;; Idempotent: untracking an already-released ctx-id is a no-op.
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

;; Worker affinity + per-library context isolation + the disposer-drain gate.
;; These exercise pool.cljc's most consumer-load-bearing machinery — worker
;; pinning and joint-pool teardown — using fake owners and release-fns, no
;; real worker-router pool (so the assertions are deterministic rather than
;; racing real workers).

(deftest worker-idx-from-args-resolves-affinity-and-falls-back
  (let [lib "affinity-test"]
    (register_library_context_BANG_ lib)
    ;; An arg carrying .worker_idx (the munged :worker-idx convention a
    ;; consumer tags onto its context returns) pins the call to that worker.
    (is (= 3 (worker_idx_from_args lib #js [#js {:worker_idx 3} "scalar"])))
    ;; Library-pure args (raw scalars) carry no index → fallback 0. The 0 is
    ;; a deliberate deterministic pin, not an any() opportunity: dispatch
    ;; cannot tell a pure call from one touching worker-local module state.
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
  ;; Joint-pool guarantee: two handler-keys sharing one pool keep independent
  ;; context registries. Same ctx-id in both libraries maps to different
  ;; workers, and resetting one library does not disturb the other.
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
  ;; The TOCTOU gate that context teardown depends on: a parent context must not
  ;; consider its children drained until every child's async release-fn Promise
  ;; has actually settled — not merely when the child was dissoc'd from the live
  ;; map. Register two child handles under one parent whose release-fns return
  ;; controllable Promises, fire explicit disposes, and assert the in-flight
  ;; count (and await-parent-drain!) stay open until both Promises resolve.
  (let [lib "drain-test"
        parent 100]
    (register_library_context_BANG_ lib)
    (let [resolvers (atom [])
          pending-release (fn []
                            (js/Promise. (fn [res _reject]
                                           (swap! resolvers conj res))))
          owner-1 #js {:k 1}
          owner-2 #js {:k 2}]
      ;; Pass the fn itself (not a call): the wrapped disposer invokes it, and
      ;; each invocation mints a fresh pending Promise whose resolver we capture.
      (register_handle_BANG_ lib 1 0 pending-release owner-1 parent)
      (register_handle_BANG_ lib 2 0 pending-release owner-2 parent)
      (is (= 2 (in_flight_count_for_parent parent)) "both children counted")
      ;; Explicit dispose runs each release-fn (capturing its resolver) but the
      ;; Promise is still pending, so the gate must NOT have decremented yet.
      (dispose_handle_BANG_ lib 1)
      (dispose_handle_BANG_ lib 2)
      (is (= 2 (count @resolvers)) "both release-fns were invoked")
      (is (= 2 (in_flight_count_for_parent parent))
          "gate stays open while release Promises are unsettled (closes the TOCTOU window)")
      (let [drained (atom false)
            drain-p (.then (await_parent_drain_BANG_ parent)
                           (fn [_] (reset! drained true)))]
        ;; Settle both release Promises; each .finally fires the decrement.
        (doseq [r @resolvers] (r nil))
        (await drain-p)
        (is (= 0 (in_flight_count_for_parent parent)) "gate closes after both settle")
        (is (true? @drained) "await-parent-drain! resolved only after all releases settled"))
      (reset_library_context_BANG_ lib))))

;; Bounded-LRU eviction, deterministic: owners stay strongly reachable, so
;; these fail if the owner-alive gate is restored (eviction would refuse).

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
    (ref_handle_BANG_ lib "p")                        ; refcount 1
    (is (= "none-evictable" (evict_oldest_BANG_ lib)) "refcount>0 pins the entry")
    (unref_handle_BANG_ lib "p")                      ; refcount 0
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

;; Run on module load: deftest forms above register at top level;
;; run-tests-and-exit! iterates the registry, awaits any Promise each
;; ^:async test returns, and process.exit-s 0 on green / 1 on failure.
(tr/run-tests-and-exit! "net.willcohen.native.pool-test")
