;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
;;
;; Uses squint's cljs.test adapter (node_modules/squint-cljs/src/squint/test.js):
;; (deftest ^:async name body) returns a Promise that test_var awaits.
;;
;; Tests the handler runtime: a structural module wrapping a methods object
;; with workerQueue serialization, an in-flight counter, a destroy
;; barrier, and an idempotent-init async-factory shape.

(ns net.willcohen.native.handler-runtime-test
  (:require [cljs.test :refer [deftest is testing]]
            ["ffi-wasm/handler-runtime"
             :refer [makeHandler
                     byteLengthFingerprint
                     normalizeWasmError
                     setLogConfig
                     getLogConfig
                     dbg]]
            ["ffi-wasm/test-runner" :as tr]
            ["node:child_process" :refer [spawnSync]]
            ["node:perf_hooks" :refer [performance]]))

;; Capture console.log output for substrate tests, restoring after each.
;; dbg() uses console.log; we intercept transparently and let assertions
;; inspect the captured lines. The reset-substrate! helper guards
;; against cross-test bleed since logState is module-level singleton.
(defn install-log-capture! []
  (let [captured #js []
        original (.-log js/console)]
    (set! (.-log js/console)
          (fn [& args] (.push captured (.join args " "))))
    {:captured captured
     :restore (fn [] (set! (.-log js/console) original))}))

(defn capture-logs [f]
  (let [{:keys [captured restore]} (install-log-capture!)]
    (try
      (f)
      {:captured captured}
      (finally (restore)))))

(defn reset-substrate! [] (setLogConfig nil))

(defn ^:async sleep [ms]
  (js/Promise. (fn [resolve _reject]
                 (js/setTimeout resolve ms))))

;; Factory shape: validation, idempotence, fingerprint.

(deftest makeHandler-with-no-methods-rejects
  (is (thrown-with-msg? js/Error #"methods"
                        (makeHandler #js {})))
  (is (thrown-with-msg? js/Error #"methods"
                        (makeHandler #js {:methods #js {}}))))

(deftest makeHandler-rejects-a-method-listed-as-both-busy-and-destroy
  ;; wrap() tests destroy before busy, so an overlapping name silently took
  ;; the destroy path and lost its busy accounting. Reject it instead.
  (is (thrown-with-msg? js/Error #"both busyMethods and destroyMethods"
                        (makeHandler #js {:methods #js {:ccall (fn ^:async ccall [] "ok")}
                                          :busyMethods #js ["ccall"]
                                          :destroyMethods #js ["ccall"]})))
  (testing "disjoint lists are still accepted"
    (is (some? (makeHandler #js {:methods #js {:a (fn ^:async a-fn [] 1)
                                               :b (fn ^:async b-fn [] 2)}
                                 :busyMethods #js ["a"]
                                 :destroyMethods #js ["b"]})))))

(deftest ^:async idempotent-re-init-with-same-args-returns-cached-handler
  (let [init-count (atom 0)
        factory (makeHandler
                 #js {:init (fn ^:async init-fn [_args] (swap! init-count inc))
                      :methods #js {:ping (fn ^:async ping [] "pong")}})
        a (await (factory #js {:x 1}))
        b (await (factory #js {:x 1}))]
    (is (= a b))
    (is (= 1 @init-count))
    (is (= "pong" (await (.ping a))))))

(deftest ^:async re-init-with-different-args-throws
  (let [factory (makeHandler
                 #js {:init (fn ^:async init-fn [] nil)
                      :methods #js {:ping (fn ^:async ping [] "pong")}})]
    (await (factory #js {:x 1}))
    (try
      (await (factory #js {:x 2}))
      (is false "should reject")
      (catch :default e
        (is (re-find #"(?i)fingerprint|init" (.-message e)))))))

;; The default fingerprint over an init payload carrying bytes. A wasm library's
;; payload is a database, so this is the shape every real consumer hits.

(defn- ping-factory [fp]
  (makeHandler (cond-> #js {:init (fn ^:async init-fn [] nil)
                            :methods #js {:ping (fn ^:async ping [] "pong")}}
                 fp (doto (aset "fingerprint" fp)))))

(deftest ^:async default-fingerprint-reduces-bytes-to-a-length
  (let [factory (ping-factory nil)
        big (js/Uint8Array. 4096)]
    (await (factory #js {:dbBytes big :logLevel 0}))
    (testing "the same payload re-inits from cache"
      (is (some? (await (factory #js {:dbBytes big :logLevel 0})))))
    (testing "a payload of the same length but different content still matches"
      (is (some? (await (factory #js {:dbBytes (js/Uint8Array. 4096) :logLevel 0})))
          "bytes contribute their length only, which is the deliberate trade"))))

(deftest ^:async default-fingerprint-separates-two-different-array-buffers
  (let [factory (ping-factory nil)]
    (await (factory #js {:dbBytes (js/ArrayBuffer. 8)}))
    (testing "an ArrayBuffer is a byte carrier, so its length discriminates"
      (try
        (await (factory #js {:dbBytes (js/ArrayBuffer. 16)}))
        (is false "should reject: 8 bytes and 16 bytes are different payloads")
        (catch :default e
          (is (re-find #"(?i)different args" (.-message e))))))))

(deftest ^:async default-fingerprint-keeps-discriminating-non-byte-fields
  (let [factory (ping-factory nil)
        db (js/Uint8Array. 8)]
    (await (factory #js {:dbBytes db :logLevel 0}))
    (testing "a scalar beside the bytes still separates two payloads"
      (try
        (await (factory #js {:dbBytes db :logLevel 2}))
        (is false "should reject")
        (catch :default e
          (is (re-find #"(?i)different args" (.-message e))))))))

(deftest ^:async default-fingerprint-finds-bytes-nested-below-the-top-level
  ;; The walk has to descend, not just check the top level. A nested
  ;; ArrayBuffer is the case that proves it: left alone it serializes as {} at
  ;; any depth, so two different payloads would compare equal. A nested scalar
  ;; would not prove it, because JSON.stringify already discriminates those.
  (let [factory (ping-factory nil)]
    (await (factory #js {:resources #js {:db (js/ArrayBuffer. 8)}}))
    (try
      (await (factory #js {:resources #js {:db (js/ArrayBuffer. 16)}}))
      (is false "should reject: 8 nested bytes and 16 are different payloads")
      (catch :default e
        (is (re-find #"(?i)different args" (.-message e)))))))

(deftest ^:async default-fingerprint-keeps-a-plain-array-s-contents
  (let [factory (ping-factory nil)]
    (await (factory #js {:sizes #js [1 2 3]}))
    (testing "a plain array has no byteLength, so it is not treated as bytes"
      (try
        (await (factory #js {:sizes #js [4 5 6]}))
        (is false "should reject: same length, different contents")
        (catch :default e
          (is (re-find #"(?i)different args" (.-message e))))))))

(deftest ^:async default-fingerprint-discriminates-BigInt-fields
  ;; JSON.stringify throws on a BigInt. The old catch fell back to
  ;; String(args), which prints "[object Object]" for every such payload, so
  ;; two different payloads compared equal and the re-init guard passed
  ;; silently. A wasm init payload carries BigInt for 64-bit sizes and
  ;; pointers, so this is a reachable shape, not a contrived one.
  (let [factory (ping-factory nil)]
    (await (factory #js {:size (js/BigInt 1) :mode "alpha"}))
    (testing "the same BigInt payload still re-inits from cache"
      (is (some? (await (factory #js {:size (js/BigInt 1) :mode "alpha"})))))
    (testing "a different BigInt payload is rejected"
      (try
        (await (factory #js {:size (js/BigInt 2) :mode "bravo"}))
        (is false "should reject: two different payloads must not compare equal")
        (catch :default e
          (is (re-find #"(?i)different args" (.-message e))))))))

(deftest ^:async default-fingerprint-throws-on-args-it-cannot-print
  ;; A payload the walk cannot reduce has no discriminating print, so the
  ;; factory fails at init rather than accept a fingerprint that collides
  ;; with every other unprintable payload. byteLengthFingerprint is the
  ;; escape hatch, and the message names it.
  (let [cyclic #js {:a #js {:b #js {:c #js {:d #js {}}}}}]
    (aset (.-d (.-c (.-b (.-a cyclic)))) "back" cyclic)
    (let [factory (ping-factory nil)]
      (try
        (await (factory cyclic))
        (is false "should reject an unfingerprintable payload")
        (catch :default e
          (is (re-find #"(?i)not fingerprintable" (.-message e)))
          (is (re-find #"byteLengthFingerprint" (.-message e))
              "the error names the escape hatch"))))
    (testing "an explicit fingerprint still accepts that payload"
      (let [factory (ping-factory (byteLengthFingerprint #js ["tag"]))]
        (aset cyclic "tag" "x")
        (is (some? (await (factory cyclic))))))))

(deftest byte-length-fingerprint-prints-named-fields-only
  (let [fp (byteLengthFingerprint #js ["dbBytes" "iniBytes" "logLevel"])]
    (is (= "dbBytes:4096|iniBytes:12|logLevel:0"
           (fp #js {:dbBytes (js/Uint8Array. 4096)
                    :iniBytes (js/Uint8Array. 12)
                    :logLevel 0})))
    (testing "a field the list omits cannot change the print"
      (is (= (fp #js {:dbBytes (js/Uint8Array. 8) :ignored "a"})
             (fp #js {:dbBytes (js/Uint8Array. 8) :ignored "b"}))))
    (testing "a missing field prints as absent rather than vanishing"
      (is (= "dbBytes:undefined|iniBytes:undefined|logLevel:undefined" (fp #js {})))
      (is (= "dbBytes:undefined|iniBytes:undefined|logLevel:undefined" (fp nil))))))

(deftest byte-length-fingerprint-prefixes-and-tolerates-a-plain-array
  (let [fp (byteLengthFingerprint #js ["dbBytes"] "gdal")]
    (is (= "gdal|dbBytes:3" (fp #js {:dbBytes (js/Uint8Array. 3)}))
        "the prefix labels the handler in the re-init message")
    (is (= "gdal|dbBytes:3" (fp #js {:dbBytes #js [1 2 3]}))
        "a named field takes a plain array's length as its byte length")))

(deftest byte-length-fingerprint-needs-a-non-empty-field-list
  (is (thrown-with-msg? js/Error #"fields must be" (byteLengthFingerprint #js [])))
  (is (thrown-with-msg? js/Error #"fields must be" (byteLengthFingerprint nil)))
  (is (thrown-with-msg? js/Error #"fields must be" (byteLengthFingerprint "dbBytes"))))

;; workerQueue serialization + destroy barrier.

(deftest ^:async busy-methods-execute-serially-via-workerQueue
  (let [events #js []
        busy-a (fn ^:async busy-a-fn []
                 (.push events #js {:kind "start" :name "A" :t (.now performance)})
                 (await (sleep 20))
                 (.push events #js {:kind "end" :name "A" :t (.now performance)}))
        busy-b (fn ^:async busy-b-fn []
                 (.push events #js {:kind "start" :name "B" :t (.now performance)})
                 (await (sleep 20))
                 (.push events #js {:kind "end" :name "B" :t (.now performance)}))
        factory (makeHandler
                 #js {:methods #js {:a busy-a :b busy-b}
                      :busyMethods #js ["a" "b"]})
        h (await (factory))]
    (await (js/Promise.all #js [(.a h) (.b h) (.a h) (.b h)]))
    (is (= 8 (.-length events)))
    (loop [i 0]
      (when (< i (.-length events))
        (is (= "start" (.-kind (aget events i))))
        (is (= "end"   (.-kind (aget events (inc i)))))
        (is (= (.-name (aget events i)) (.-name (aget events (inc i)))))
        (recur (+ i 2))))))

;; The two tests below record the order methods enter and leave, rather than
;; measuring how long the run took. Order is what the gate actually promises,
;; and a loaded runner cannot stretch it the way it stretches a wall-clock
;; window. The stress test further down keeps its timestamps, because it
;; compares two measured events to each other rather than to a fixed budget.

(deftest ^:async destroy-method-waits-for-in-flight-busy-methods
  (let [events #js []
        factory (makeHandler
                 #js {:methods
                      #js {:busy    (fn ^:async busy []
                                      (.push events "busy-enter")
                                      (await (sleep 50))
                                      (.push events "busy-exit"))
                           :destroy (fn ^:async destroy []
                                      (.push events "destroy-enter"))}
                      :busyMethods    #js ["busy"]
                      :destroyMethods #js ["destroy"]})
        h (await (factory))
        busy-p (.busy h)]
    ;; schedule destroy after busy enters
    (await (sleep 5))
    (let [destroy-p (.destroy h)]
      (await (js/Promise.all #js [busy-p destroy-p])))
    (is (= "busy-enter,busy-exit,destroy-enter" (.join events ","))
        "destroy must not enter while a busy method is still in flight")))

(deftest ^:async destroy-method-does-NOT-wait-for-other-destroys-via-the-counter
  (let [events #js []
        factory (makeHandler
                 #js {:methods
                      #js {:d1 (fn ^:async d1 []
                                 (.push events "d1-enter")
                                 (await (sleep 30))
                                 (.push events "d1-exit"))
                           :d2 (fn ^:async d2 []
                                 (.push events "d2-enter")
                                 (await (sleep 30))
                                 (.push events "d2-exit"))}
                      :destroyMethods #js ["d1" "d2"]})
        h  (await (factory))]
    (await (js/Promise.all #js [(.d1 h) (.d2 h)]))
    ;; Interleaved entries would read d1-enter,d2-enter,...; the serial queue
    ;; runs one destroy to completion before the next starts.
    (is (= "d1-enter,d1-exit,d2-enter,d2-exit" (.join events ","))
        "the destroys run one after the other, neither waiting on a counter")))

(deftest ^:async barrier-survives-N-concurrent-busy-plus-1-destroy-stress-run
  (let [n 50
        exits #js []
        destroy-entry (atom 0)
        factory (makeHandler
                 #js {:methods
                      #js {:work     (fn ^:async work []
                                       (let [ms (+ 1 (.floor js/Math (* (.random js/Math) 10)))]
                                         (await (sleep ms))
                                         (.push exits (.now performance))))
                           :teardown (fn ^:async teardown []
                                       (reset! destroy-entry (.now performance)))}
                      :busyMethods    #js ["work"]
                      :destroyMethods #js ["teardown"]})
        h (await (factory))
        busy #js []]
    (dotimes [_ n] (.push busy (.work h)))
    ;; schedule destroy at a random offset so it interleaves with busy calls
    (let [destroy-p ((fn ^:async schedule-destroy []
                       (await (sleep (.floor js/Math (* (.random js/Math) 5))))
                       (await (.teardown h))))]
      (await (js/Promise.all (.concat busy #js [destroy-p]))))
    (let [last-exit (.apply (.-max js/Math) nil exits)]
      (is (= n (.-length exits)))
      (is (>= @destroy-entry last-exit)
          (str "destroy entry " @destroy-entry
               " must be >= last busy exit " last-exit)))))

;; normalizeWasmError + wrap normalization.

(deftest normalizeWasmError-converts-WebAssembly-RuntimeError-to-plain-Error-with-wasmTrap-tag
  (let [original (new js/WebAssembly.RuntimeError "memory access out of bounds")
        out (normalizeWasmError original)]
    (is (= false (instance? js/WebAssembly.RuntimeError out))
        "must downcast to plain Error so structuredClone-across-RPC works")
    (is (instance? js/Error out))
    (is (= true (.-wasmTrap out)))
    (is (= "wasm-runtime-error" (.-kind out)))
    (is (re-find #"memory access out of bounds" (.-message out)))
    (is (= "string" (js* "typeof ~{}" (.-stack out))))))

(deftest normalizeWasmError-converts-emscripten-Aborted-to-plain-Error-with-wasmTrap-tag
  (let [original (js/Error. "Aborted(native code called abort())")
        out (normalizeWasmError original)]
    (is (= true (.-wasmTrap out)))
    (is (= "emscripten-abort" (.-kind out)))
    (is (re-find #"^Aborted" (.-message out)))))

(deftest normalizeWasmError-passes-through-plain-Errors-unchanged
  (let [original (js/Error. "domain error")
        out (normalizeWasmError original)]
    (is (= out original) "plain Errors must pass through identity")
    (is (= js/undefined (.-wasmTrap out)))))

(deftest ^:async wrap-normalizes-synchronous-wasm-RuntimeError-thrown-from-a-destroy-method
  (let [factory (makeHandler
                 #js {:methods #js {:teardown
                                    (fn []
                                      (throw (new js/WebAssembly.RuntimeError "OOB sync")))}
                      :destroyMethods #js ["teardown"]})
        h (await (factory))]
    (try
      (await (.teardown h))
      (is false "should reject")
      (catch :default e
        (is (= false (instance? js/WebAssembly.RuntimeError e)))
        (is (= true (.-wasmTrap e)))
        (is (= "wasm-runtime-error" (.-kind e)))
        (is (re-find #"OOB sync" (.-message e)))))))

(deftest ^:async wrap-normalizes-async-wasm-RuntimeError-thrown-from-a-busy-method
  (let [factory (makeHandler
                 #js {:methods #js {:work
                                    (fn ^:async work []
                                      (await (sleep 1))
                                      (throw (new js/WebAssembly.RuntimeError "OOB async busy")))}
                      :busyMethods #js ["work"]})
        h (await (factory))]
    (try
      (await (.work h))
      (is false "should reject")
      (catch :default e
        (is (= true (.-wasmTrap e)))
        (is (= "wasm-runtime-error" (.-kind e)))
        (is (re-find #"OOB async busy" (.-message e)))))))

(deftest ^:async wrap-normalizes-async-emscripten-Aborted-thrown-from-a-default-classified-method
  (let [factory (makeHandler
                 #js {:methods #js {:misc
                                    (fn ^:async misc []
                                      (await (sleep 1))
                                      (throw (js/Error. "Aborted(native code called abort())")))}})
        h (await (factory))]
    (try
      (await (.misc h))
      (is false "should reject")
      (catch :default e
        (is (= true (.-wasmTrap e)))
        (is (= "emscripten-abort" (.-kind e)))
        (is (re-find #"^Aborted" (.-message e)))))))

(deftest ^:async wrap-leaves-the-workerQueue-alive-after-a-wasm-trap
  ;; A rejected method must NOT poison the queue. The next call must run,
  ;; not stall on a rejected predecessor.
  (let [order #js []
        factory (makeHandler
                 #js {:methods #js {:trap (fn ^:async trap []
                                            (throw (new js/WebAssembly.RuntimeError "OOB")))
                                    :ok   (fn ^:async ok []
                                            (.push order "ok")
                                            "fine")}
                      :busyMethods #js ["trap" "ok"]})
        h (await (factory))]
    (try
      (await (.trap h))
      (is false "trap call should reject")
      (catch :default _e nil))
    (let [result (await (.ok h))]
      (is (= "fine" result))
      (is (= 1 (.-length order)))
      (is (= "ok" (aget order 0))))))

;; Diagnostic substrate (setLogConfig / dbg / factory propagation).

(deftest setLogConfig-default-state-is-off-dbg-is-silent
  (reset-substrate!)
  (is (= #js {:level nil :categories nil} (getLogConfig)))
  (let [{:keys [captured]} (capture-logs (fn [] (dbg "BUSY-INC" #js {:fn "x" :counter 1})))]
    (is (= 0 (.-length captured)) "no console.log when level is null")))

(deftest setLogConfig-level-debug-enables-emission-with-default-category-filter
  (reset-substrate!)
  (setLogConfig #js {:level "debug"})
  (is (= #js {:level "debug" :categories nil} (getLogConfig)))
  (let [{:keys [captured]} (capture-logs
                            (fn []
                              (dbg "BUSY-INC"     #js {:fn "x" :counter 1})
                              (dbg "FR-CALLBACK"  #js {:ctxId 7})
                              (dbg "DESTROY-FIRE" #js {:fn "d" :busy 0})))]
    (is (= 3 (.-length captured)))
    (is (re-find #"\[CLJ-NATIVE ts=\d+ BUSY-INC\] fn=x counter=1" (aget captured 0)))
    (is (re-find #"\[CLJ-NATIVE ts=\d+ FR-CALLBACK\] ctxId=7"     (aget captured 1)))
    (is (re-find #"\[CLJ-NATIVE ts=\d+ DESTROY-FIRE\] fn=d busy=0" (aget captured 2)))))

(deftest setLogConfig-null-resets-to-off
  (setLogConfig #js {:level "debug" :categories #js ["busy"]})
  (setLogConfig nil)
  (is (= #js {:level nil :categories nil} (getLogConfig)))
  (let [{:keys [captured]} (capture-logs (fn [] (dbg "BUSY-INC" #js {})))]
    (is (= 0 (.-length captured)))))

(deftest setLogConfig-categories-filter-by-tag-prefix-lowercased-before-first-dash
  (reset-substrate!)
  (setLogConfig #js {:level "debug" :categories #js ["busy" "fr"]})
  (is (= #js {:level "debug" :categories #js ["busy" "fr"]} (getLogConfig)))
  (let [{:keys [captured]} (capture-logs
                            (fn []
                              (dbg "BUSY-INC"               #js {})  ; busy → pass
                              (dbg "FR-CALLBACK-SUPPRESSED" #js {})  ; fr → pass
                              (dbg "DESTROY-FIRE"           #js {})  ; destroy → drop
                              (dbg "QUEUE-DISPATCH"         #js {})  ; queue → drop
                              (dbg "EXPLICIT-DISPOSE"       #js {})))]; explicit → drop
    (is (= 2 (.-length captured)))
    (is (re-find #"BUSY-INC"               (aget captured 0)))
    (is (re-find #"FR-CALLBACK-SUPPRESSED" (aget captured 1)))))

(deftest setLogConfig-keys-absent-from-cfg-leave-existing-state-untouched
  (reset-substrate!)
  (setLogConfig #js {:level "debug" :categories #js ["busy"]})
  (setLogConfig #js {:categories #js ["fr"]}) ; level untouched
  (is (= #js {:level "debug" :categories #js ["fr"]} (getLogConfig))))

(deftest setLogConfig-level-rank-event-level-above-config-level-is-suppressed
  (reset-substrate!)
  (setLogConfig #js {:level "info"}) ; info=3, debug=4, trace=5
  (let [{:keys [captured]} (capture-logs
                            (fn []
                              (dbg "A" #js {} "error") ; 1 <= 3, pass
                              (dbg "B" #js {} "warn")  ; 2 <= 3, pass
                              (dbg "C" #js {} "info")  ; 3 <= 3, pass
                              (dbg "D" #js {} "debug") ; 4 > 3, drop
                              (dbg "E" #js {} "trace")))]; 5 > 3, drop
    (is (= 3 (.-length captured)))))

(deftest setLogConfig-invalid-level-throws
  (reset-substrate!)
  (is (thrown-with-msg? js/Error #"level must be one of"
                        (setLogConfig #js {:level "verbose"})))
  (is (thrown-with-msg? js/Error #"level must be one of"
                        (setLogConfig #js {:level 42}))))

(deftest setLogConfig-invalid-categories-shape-throws
  (reset-substrate!)
  (is (thrown-with-msg? js/Error #"categories must be an array"
                        (setLogConfig #js {:categories "busy"})))
  (is (thrown-with-msg? js/Error #"categories must be an array"
                        (setLogConfig #js {:categories #js {:busy true}}))))

(deftest setLogConfig-rejected-cfg-leaves-the-previous-state-intact
  ;; Both fields are validated before either is written. Applying as we go
  ;; committed a valid `level` and then threw on `categories`, so the caller
  ;; saw a failure while the log state had already moved.
  (reset-substrate!)
  (is (thrown-with-msg? js/Error #"categories must be an array"
                        (setLogConfig #js {:level "debug" :categories 42})))
  (is (nil? (.-level (getLogConfig)))
      "a rejected call must not commit the level it validated first")
  (testing "the same guard holds over an already-configured state"
    (setLogConfig #js {:level "warn" :categories #js ["busy"]})
    (is (thrown-with-msg? js/Error #"categories must be an array"
                          (setLogConfig #js {:level "trace" :categories 42})))
    (is (= "warn" (.-level (getLogConfig))))
    (is (= 1 (.-length (.-categories (getLogConfig)))))
    (reset-substrate!)))

(deftest setLogConfig-Set-is-accepted-for-categories-deduped-lowercased
  (reset-substrate!)
  (setLogConfig #js {:level "debug"
                     :categories (new js/Set #js ["Busy" "BUSY" "fr"])})
  (let [cfg (getLogConfig)]
    (is (= 2 (.-length (.-categories cfg))))
    (is (.includes (.-categories cfg) "busy"))
    (is (.includes (.-categories cfg) "fr"))))

(deftest ^:async factory-propagates-initArgs-handlerRuntime-to-setLogConfig-before-wrap-fires
  (reset-substrate!)
  (let [factory (makeHandler
                 #js {:init (fn ^:async init-fn [] nil)
                      :methods #js {:ping (fn ^:async ping [] "pong")}
                      :busyMethods #js ["ping"]
                      :label "rt-test"})
        {:keys [captured restore]} (install-log-capture!)]
    (try
      (let [h (await (factory #js {:handlerRuntime
                                      #js {:logLevel "debug"
                                           :logCategories #js ["busy"]}}))]
        (is (= #js {:level "debug" :categories #js ["busy"]} (getLogConfig)))
        (await (.ping h)))
      (finally (restore)))
    (let [busy-events (.filter captured (fn [l] (re-find #"\[CLJ-NATIVE" l)))]
      (is (= 2 (.-length busy-events)))
      (is (re-find #"BUSY-INC.*fn=ping.*label=rt-test.*counter=1" (aget busy-events 0)))
      (is (re-find #"BUSY-DEC.*fn=ping.*label=rt-test.*counter=0" (aget busy-events 1))))
    (reset-substrate!)))

(deftest ^:async ctx-attachEmscriptenModule-wires-built-in-heap-inspector
  (reset-substrate!)
  (setLogConfig #js {:level "debug" :categories #js ["busy"]})
  (let [mock-module #js {:HEAPU8 #js {:length 65536}
                         :_sbrk  (fn [_n] 32768)}
        factory (makeHandler
                 #js {:init (fn ^:async init-fn [_initArgs ctx]
                              (.attachEmscriptenModule ctx mock-module))
                      :methods #js {:ping (fn ^:async ping [] "pong")}
                      :busyMethods #js ["ping"]
                      :label "heap-test"})
        {:keys [captured restore]} (install-log-capture!)]
    (try
      (let [h (await (factory #js {}))]
        (await (.ping h)))
      (finally
        (restore)
        (reset-substrate!)))
    (let [busy-events (.filter captured (fn [l] (re-find #"\[CLJ-NATIVE" l)))]
      (is (= 2 (.-length busy-events)))
      ;; heap-bytes from HEAPU8.length, brk from _sbrk(0) result.
      (is (re-find #"BUSY-INC.*heap-bytes=65536.*brk=32768" (aget busy-events 0)))
      (is (re-find #"BUSY-DEC.*heap-bytes=65536.*brk=32768" (aget busy-events 1))))))

(deftest ^:async no-ctx-attachEmscriptenModule-call-BUSY-events-carry-no-heap-fields
  (reset-substrate!)
  (setLogConfig #js {:level "debug" :categories #js ["busy"]})
  (let [factory (makeHandler
                 #js {:init (fn ^:async init-fn [] nil) ; does NOT attach a module
                      :methods #js {:ping (fn ^:async ping [] "pong")}
                      :busyMethods #js ["ping"]
                      :label "no-heap-test"})
        {:keys [captured restore]} (install-log-capture!)]
    (try
      (let [h (await (factory #js {}))]
        (await (.ping h)))
      (finally
        (restore)
        (reset-substrate!)))
    (let [busy-events (.filter captured (fn [l] (re-find #"\[CLJ-NATIVE" l)))]
      (is (= 2 (.-length busy-events)))
      (is (not (re-find #"heap-bytes" (aget busy-events 0))))
      (is (not (re-find #"brk="       (aget busy-events 0)))))))

(deftest ^:async emscripten-inspector-tolerates-module-without-_sbrk
  (reset-substrate!)
  (setLogConfig #js {:level "debug" :categories #js ["busy"]})
  (let [partial-module #js {:HEAPU8 #js {:length 1024}} ; no _sbrk
        factory (makeHandler
                 #js {:init (fn ^:async init-fn [_initArgs ctx]
                              (.attachEmscriptenModule ctx partial-module))
                      :methods #js {:ping (fn ^:async ping [] "pong")}
                      :busyMethods #js ["ping"]
                      :label "partial-heap-test"})
        {:keys [captured restore]} (install-log-capture!)]
    (try
      (let [h (await (factory #js {}))]
        (await (.ping h)))
      (finally
        (restore)
        (reset-substrate!)))
    (let [busy (-> (.filter captured (fn [l] (re-find #"\[CLJ-NATIVE" l)))
                   (aget 0))]
      (is (re-find #"heap-bytes=1024" busy))
      (is (not (re-find #"brk=" busy))))))

(deftest ^:async busy-wrap-skips-heap-probe-when-substrate-is-off
  ;; Regression: the BUSY-INC/BUSY-DEC event object — and the inspectHeap()
  ;; call embedded in it, which invokes the module's _sbrk(0) (a wasm boundary
  ;; crossing) — must be built only when the substrate is enabled. With
  ;; logging off (the steady-state default, and every ccall runs this path
  ;; since ccall is a busy method), a busy call must not touch _sbrk
  ;; at all. Before the guard, the field object was constructed eagerly as the
  ;; dbgPaired argument regardless of the enabled flag.
  (reset-substrate!)
  (let [sbrk-calls (atom 0)
        mock-module #js {:HEAPU8 #js {:length 65536}
                         :_sbrk (fn [_n] (swap! sbrk-calls inc) 32768)}
        factory (makeHandler
                 #js {:init (fn ^:async init-fn [_initArgs ctx]
                              (.attachEmscriptenModule ctx mock-module))
                      :methods #js {:ping (fn ^:async ping [] "pong")}
                      :busyMethods #js ["ping"]
                      :label "sbrk-gate-test"})
        h (await (factory #js {}))]
    ;; Substrate off: one busy call must probe the heap zero times.
    (await (.ping h))
    (is (= 0 @sbrk-calls)
        "busy wrap must not call _sbrk(0) when the substrate is off")
    ;; Substrate on: the same call probes the heap twice (BUSY-INC + BUSY-DEC),
    ;; confirming the guard opens the path rather than removing it.
    (setLogConfig #js {:level "debug" :categories #js ["busy"]})
    (let [{:keys [restore]} (install-log-capture!)]
      (try (await (.ping h))
           (finally (restore))))
    (is (= 2 @sbrk-calls)
        "busy wrap probes the heap once per emitted BUSY event when on")
    (reset-substrate!)))

(deftest ^:async init-failure-rolls-state-back-so-subsequent-calls-can-retry
  ;; First call fails inside init (e.g. validates required args); the
  ;; runtime must clear cachedFingerprint and initPromise so a follow-up
  ;; call with different (corrected) args starts fresh, instead of being
  ;; rejected with "re-init with different args while init in flight".
  (let [attempts (atom 0)
        factory (makeHandler
                 #js {:init (fn ^:async init-fn [args]
                              (swap! attempts inc)
                              (when-not (.-ok args)
                                (throw (js/Error. "missing required arg `ok`"))))
                      :methods #js {:ping (fn ^:async ping [] "pong")}})]
    (try
      (await (factory #js {:ok false}))
      (is false "first call should reject")
      (catch :default e
        (is (re-find #"missing required arg" (.-message e)))))
    ;; After the failure, retry with corrected args must succeed.
    (let [h (await (factory #js {:ok true}))]
      (is (= "pong" (await (.ping h)))))
    (is (= 2 @attempts)
        "init must run twice: once for the failed attempt, once for the retry")))

;; Run on module load: deftest forms above register at top level;
;; run-tests-and-exit! iterates the registry, awaits any Promise each
;; ^:async test returns, and process.exit-s 0 on green / 1 on failure.
(deftest importing-the-runtime-keeps-the-default-exit
  ;; A pool worker that crashes must exit nonzero, or worker-router cannot
  ;; see it.
  (doseq [src ["setTimeout(() => { throw new Error('boom'); }, 0);"
               "Promise.reject(new Error('boom'));"]]
    (let [r (spawnSync "node"
                   #js ["--input-type=module" "-e"
                        (str "await import('ffi-wasm/handler-runtime'); " src)]
                   #js {:encoding "utf8"})]
      (is (= 1 (.-status r)) src))))

(tr/run-tests-and-exit! "net.willcohen.native.handler-runtime-test")
