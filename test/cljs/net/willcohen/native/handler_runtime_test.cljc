;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.handler-runtime-test
  (:require [cljs.test :refer [deftest is testing]]
            ["ffi-wasm/handler-runtime"
             :refer [makeHandler
                     byteLengthFingerprint
                     normalizeWasmError
                     setLogConfig
                     isEnabled
                     dbg]]
            ["ffi-wasm/test-runner" :as tr]
            ["node:child_process" :refer [spawnSync]]))

;; dbg() writes through console.log. logState is a module-level singleton, so
;; a test that sets it resets it with (setLogConfig nil).
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

(deftest makeHandler-with-no-methods-rejects
  (is (thrown-with-msg? js/Error #"methods"
                        (makeHandler #js {})))
  (is (thrown-with-msg? js/Error #"methods"
                        (makeHandler #js {:methods #js {}}))))

(deftest makeHandler-without-a-fingerprint-rejects
  (is (thrown-with-msg? js/Error #"fingerprint"
                        (makeHandler #js {:methods #js {:ping (fn ^:async ping [] "pong")}}))))

(deftest makeHandler-rejects-a-method-listed-as-both-busy-and-destroy
  ;; wrap() tests destroy before busy, so an overlapping name would lose its
  ;; busy accounting.
  (is (thrown-with-msg? js/Error #"both busyMethods and destroyMethods"
                        (makeHandler #js {:methods #js {:ccall (fn ^:async ccall [] "ok")}
                                          :fingerprint (fn [] "")
                                          :busyMethods #js ["ccall"]
                                          :destroyMethods #js ["ccall"]})))
  (testing "disjoint lists are still accepted"
    (is (some? (makeHandler #js {:fingerprint (fn [] "")
                                 :methods #js {:a (fn ^:async a-fn [] 1)
                                               :b (fn ^:async b-fn [] 2)}
                                 :busyMethods #js ["a"]
                                 :destroyMethods #js ["b"]})))))

(deftest ^:async idempotent-re-init-with-same-args-returns-cached-handler
  (let [init-count (atom 0)
        factory (makeHandler
                 #js {:fingerprint (fn [args] (str (.-x args)))
                      :init (fn ^:async init-fn [_args] (swap! init-count inc))
                      :methods #js {:ping (fn ^:async ping [] "pong")}})
        a (await (factory #js {:x 1}))
        b (await (factory #js {:x 1}))]
    (is (= a b))
    (is (= 1 @init-count))
    (is (= "pong" (await (.ping a))))))

(deftest ^:async re-init-with-different-args-throws
  (let [factory (makeHandler
                 #js {:fingerprint (fn [args] (str (.-x args)))
                      :init (fn ^:async init-fn [] nil)
                      :methods #js {:ping (fn ^:async ping [] "pong")}})]
    (await (factory #js {:x 1}))
    (try
      (await (factory #js {:x 2}))
      (is false "should reject")
      (catch :default e
        (is (re-find #"(?i)fingerprint|init" (.-message e)))))))

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

(defn- traced-method
  "An async method that pushes `label`-enter, yields to the event loop, then
   pushes `label`-exit."
  [events label]
  (fn ^:async traced []
    (.push events (str label "-enter"))
    (await (sleep 1))
    (.push events (str label "-exit"))))

(deftest ^:async every-method-runs-in-call-order-on-one-queue
  ;; A destroy waits for the busy call before it, and the next call waits for
  ;; the destroy.
  (let [events #js []
        factory (makeHandler
                 #js {:methods #js {:a     (traced-method events "a")
                                    :b     (traced-method events "b")
                                    :d1    (traced-method events "d1")
                                    :d2    (traced-method events "d2")
                                    :plain (traced-method events "plain")}
                      :fingerprint    (fn [] "")
                      :busyMethods    #js ["a" "b"]
                      :destroyMethods #js ["d1" "d2"]})
        h (await (factory))]
    (await (js/Promise.all #js [(.a h) (.d1 h) (.d2 h) (.b h) (.plain h)]))
    (is (= (str "a-enter,a-exit,d1-enter,d1-exit,d2-enter,d2-exit,"
                "b-enter,b-exit,plain-enter,plain-exit")
           (.join events ",")))))

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
                 #js {:fingerprint (fn [] "")
                      :methods #js {:teardown
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
                 #js {:fingerprint (fn [] "")
                      :methods #js {:work
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
                 #js {:fingerprint (fn [] "")
                      :methods #js {:misc
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

(deftest ^:async a-rejected-call-does-not-stall-the-queue
  (let [factory (makeHandler
                 #js {:methods #js {:trap (fn ^:async trap []
                                            (throw (new js/WebAssembly.RuntimeError "OOB")))
                                    :ok   (fn ^:async ok [] "fine")}
                      :fingerprint (fn [] "")
                      :busyMethods #js ["trap" "ok"]})
        h (await (factory))]
    (is (thrown? js/Error (await (.trap h))))
    (is (= "fine" (await (.ok h))))))

(deftest setLogConfig-default-state-is-off-dbg-is-silent
  (reset-substrate!)
  (is (not (isEnabled "BUSY-INC")))
  (let [{:keys [captured]} (capture-logs (fn [] (dbg "BUSY-INC" #js {:fn "x" :counter 1})))]
    (is (= 0 (.-length captured)) "no console.log when level is null")))

(deftest setLogConfig-level-debug-enables-emission-with-default-category-filter
  (reset-substrate!)
  (setLogConfig #js {:level "debug"})
  (is (isEnabled "BUSY-INC"))
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
  (is (not (isEnabled "BUSY-INC")))
  (let [{:keys [captured]} (capture-logs (fn [] (dbg "BUSY-INC" #js {})))]
    (is (= 0 (.-length captured)))))

(deftest setLogConfig-categories-filter-by-tag-prefix-lowercased-before-first-dash
  (reset-substrate!)
  (setLogConfig #js {:level "debug" :categories #js ["busy" "fr"]})
  (is (not (isEnabled "DESTROY-FIRE")))
  (let [{:keys [captured]} (capture-logs
                            (fn []
                              (dbg "BUSY-INC"               #js {})
                              (dbg "FR-CALLBACK-SUPPRESSED" #js {})
                              (dbg "DESTROY-FIRE"           #js {})
                              (dbg "QUEUE-DISPATCH"         #js {})
                              (dbg "EXPLICIT-DISPOSE"       #js {})))]
    (is (= 2 (.-length captured)))
    (is (re-find #"BUSY-INC"               (aget captured 0)))
    (is (re-find #"FR-CALLBACK-SUPPRESSED" (aget captured 1)))))

(deftest setLogConfig-keys-absent-from-cfg-leave-existing-state-untouched
  (reset-substrate!)
  (setLogConfig #js {:level "debug" :categories #js ["busy"]})
  (setLogConfig #js {:categories #js ["fr"]})
  (is (isEnabled "FR-CALLBACK"))
  (is (not (isEnabled "BUSY-INC"))))

(deftest setLogConfig-a-level-below-debug-suppresses-every-event
  ;; Every event is debug level.
  (reset-substrate!)
  (setLogConfig #js {:level "info"})
  (is (= 0 (.-length (:captured (capture-logs (fn [] (dbg "A" #js {})))))))
  (setLogConfig #js {:level "trace"})
  (is (= 1 (.-length (:captured (capture-logs (fn [] (dbg "A" #js {}))))))))

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
  ;; setLogConfig once wrote a valid `level` and then threw on `categories`.
  (reset-substrate!)
  (is (thrown-with-msg? js/Error #"categories must be an array"
                        (setLogConfig #js {:level "debug" :categories 42})))
  (is (not (isEnabled "BUSY-INC"))
      "a rejected call must not commit the level it validated first")
  (testing "the same guard holds over an already-configured state"
    (setLogConfig #js {:level "debug" :categories #js ["busy"]})
    (is (thrown-with-msg? js/Error #"categories must be an array"
                          (setLogConfig #js {:level "off" :categories 42})))
    (is (isEnabled "BUSY-INC"))
    (is (not (isEnabled "FR-CALLBACK")))
    (reset-substrate!)))

(deftest setLogConfig-accepts-a-Set-of-categories-in-any-case
  (reset-substrate!)
  (setLogConfig #js {:level "debug"
                     :categories (new js/Set #js ["Busy" "BUSY" "fr"])})
  (is (isEnabled "BUSY-INC"))
  (is (isEnabled "FR-CALLBACK"))
  (is (not (isEnabled "DESTROY-FIRE"))))

(deftest ^:async factory-propagates-initArgs-handlerRuntime-to-setLogConfig-before-wrap-fires
  (reset-substrate!)
  (let [factory (makeHandler
                 #js {:fingerprint (fn [] "")
                      :init (fn ^:async init-fn [] nil)
                      :methods #js {:ping (fn ^:async ping [] "pong")}
                      :busyMethods #js ["ping"]
                      :label "rt-test"})
        {:keys [captured restore]} (install-log-capture!)]
    (try
      (let [h (await (factory #js {:handlerRuntime
                                      #js {:logLevel "debug"
                                           :logCategories #js ["busy"]}}))]
        (is (isEnabled "BUSY-INC"))
        (await (.ping h)))
      (finally (restore)))
    (let [busy-events (.filter captured (fn [l] (re-find #"\[CLJ-NATIVE" l)))]
      (is (= 2 (.-length busy-events)))
      (is (re-find #"BUSY-INC.*fn=ping.*label=rt-test" (aget busy-events 0)))
      (is (re-find #"BUSY-DEC.*fn=ping.*label=rt-test" (aget busy-events 1))))
    (reset-substrate!)))

(defn- ^:async busy-lines
  "The [CLJ-NATIVE lines of one ping through a busy handler, with BUSY logging
   on. Init attaches `module` when it is not nil."
  [module]
  (setLogConfig #js {:level "debug" :categories #js ["busy"]})
  (let [factory (makeHandler
                 #js {:init (fn ^:async init-fn [_initArgs ctx]
                              (when module (.attachEmscriptenModule ctx module)))
                      :fingerprint (fn [] "")
                      :methods #js {:ping (fn ^:async ping [] "pong")}
                      :busyMethods #js ["ping"]})
        {:keys [captured restore]} (install-log-capture!)]
    (try
      (await (.ping (await (factory #js {}))))
      (finally
        (restore)
        (setLogConfig nil)))
    (.filter captured (fn [l] (re-find #"\[CLJ-NATIVE" l)))))

(deftest ^:async ctx-attachEmscriptenModule-wires-built-in-heap-inspector
  (let [lines (await (busy-lines #js {:HEAPU8 #js {:length 65536}
                                      :_sbrk  (fn [_n] 32768)}))]
    (is (= 2 (.-length lines)))
    (is (re-find #"BUSY-INC.*heap-bytes=65536.*brk=32768" (aget lines 0)))
    (is (re-find #"BUSY-DEC.*heap-bytes=65536.*brk=32768" (aget lines 1)))))

(deftest ^:async no-ctx-attachEmscriptenModule-call-BUSY-events-carry-no-heap-fields
  (let [lines (await (busy-lines nil))]
    (is (= 2 (.-length lines)))
    (is (not (re-find #"heap-bytes|brk=" (aget lines 0))))))

(deftest ^:async emscripten-inspector-tolerates-module-without-_sbrk
  (let [line (aget (await (busy-lines #js {:HEAPU8 #js {:length 1024}})) 0)]
    (is (re-find #"heap-bytes=1024" line))
    (is (not (re-find #"brk=" line)))))

(deftest ^:async busy-wrap-skips-heap-probe-when-substrate-is-off
  ;; The heap probe calls _sbrk(0), a wasm boundary crossing, and every ccall
  ;; is a busy method. Build the BUSY event only when logging is on.
  (reset-substrate!)
  (let [sbrk-calls (atom 0)
        mock-module #js {:HEAPU8 #js {:length 65536}
                         :_sbrk (fn [_n] (swap! sbrk-calls inc) 32768)}
        factory (makeHandler
                 #js {:init (fn ^:async init-fn [_initArgs ctx]
                              (.attachEmscriptenModule ctx mock-module))
                      :fingerprint (fn [] "")
                      :methods #js {:ping (fn ^:async ping [] "pong")}
                      :busyMethods #js ["ping"]
                      :label "sbrk-gate-test"})
        h (await (factory #js {}))]
    (await (.ping h))
    (is (= 0 @sbrk-calls)
        "busy wrap must not call _sbrk(0) when the substrate is off")
    (setLogConfig #js {:level "debug" :categories #js ["busy"]})
    (let [{:keys [restore]} (install-log-capture!)]
      (try (await (.ping h))
           (finally (restore))))
    (is (= 2 @sbrk-calls)
        "busy wrap probes the heap once per emitted BUSY event when on")
    (reset-substrate!)))

(defn- failing-init
  "An init with no await. It counts its runs in `attempts` and throws unless
   its args carry `ok`."
  [attempts]
  (fn init-fn [args]
    (swap! attempts inc)
    (when-not (.-ok args)
      (throw (js/Error. "missing required arg `ok`")))))

(defn- async-init
  "`init` in an async fn, so its throw becomes a rejection."
  [init]
  (fn ^:async async-init-fn [args] (init args)))

(deftest ^:async init-failure-rolls-state-back-so-subsequent-calls-can-retry
  ;; A failed init must clear the fingerprint and initPromise, so a retry with
  ;; corrected args is not rejected as a re-init. A sync init that threw once
  ;; left initPromise set, and every retry failed.
  (doseq [[why wrap-init] [["async init" async-init] ["sync init" identity]]]
    (let [attempts (atom 0)
          factory (makeHandler
                   #js {:init (wrap-init (failing-init attempts))
                        :fingerprint (fn [] "")
                        :methods #js {:ping (fn ^:async ping [] "pong")}})]
      (is (thrown-with-msg? js/Error #"missing required arg"
                            (await (factory #js {:ok false})))
          why)
      (is (= "pong" (await (.ping (await (factory #js {:ok true}))))) why)
      (is (= 2 @attempts) why))))

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
