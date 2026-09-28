;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
(ns net.willcohen.native.dispatch-test
  "Tests of dispatch.cljc under JVM clojure.test and squint cljs.test. The cljs
   build imports ffi-wasm/* by package self-resolution, since squint would look
   for the ns next to this file."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            [clojure.string :as string]
            #?(:clj  [net.willcohen.native.dispatch :as d]
               :cljs ["ffi-wasm/dispatch" :as d])
            #?(:clj [net.willcohen.native.platform])
            #?(:clj [tech.v3.datatype.ffi :as dt-ffi])
            #?(:cljs ["ffi-wasm/pool" :as pool])
            #?(:cljs ["ffi-wasm/test-runner" :as tr])))

(def number-typed-argtypes
  [:pointer :pointer? :int32 :int64 :float64 :size-t :void])

(def sample-fndefs
  {:lib_make  {:rettype :pointer :argtypes [[:ctx :pointer] [:name :string]]}
   :lib_count {:rettype :int32   :argtypes [[:ctx :pointer]]}
   :lib_free  {:rettype :void    :argtypes [[:handle :pointer]]}})

;; squint compiles :number to the string "number", so one assertion holds on
;; both platforms.
(deftest argtype->ccall-type-maps-every-known-type
  (testing "number-typed argtypes collapse to :number"
    (doseq [t number-typed-argtypes]
      (is (= :number (d/argtype->ccall-type t))
          (str t " -> :number"))))
  (testing ":string and :string? map to :string"
    (is (= :string (d/argtype->ccall-type :string)))
    (is (= :string (d/argtype->ccall-type :string?)))))

(deftest library-validates-fndefs-types-at-build-time
  ;; argtype->ccall-type maps an unknown type to :number with no error, so
  ;; `library` must reject it.
  (testing "a fn-def with supported types builds"
    (is (some? (d/library {:key :ok-lib :fndefs sample-fndefs}))))
  (testing "an unknown rettype throws at assembly, not at the first call"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (d/library {:key :bad-lib
                             :fndefs {:lib_bad {:rettype :int128
                                                :argtypes [[:x :pointer]]}}}))))
  (testing "a string-array type throws, since dt-ffi has none"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (d/library {:key :bad-lib
                             :fndefs {:lib_sa {:rettype :void
                                               :argtypes [[:x :string-array]]}}}))))
  (testing "an unknown argtype throws at assembly"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (d/library {:key :bad-lib
                             :fndefs {:lib_bad {:rettype :void
                                                :argtypes [[:x :quaternion]]}}}))))
  (testing ":string? and :int64 arguments get their indexes precomputed"
    (let [r (get-in (d/library {:key :idx-lib
                                :fndefs {:lib_mix {:rettype :int64
                                                   :argtypes [[:p :pointer] [:s :string?] [:v :int64]]}}})
                    [:fns :lib_mix])]
      (is (= :number (:ccall-rettype r)))
      (is (= :string (nth (:ccall-argtypes r) 1)))
      (is (and (= 1 (count (:string?-indexes r))) (contains? (:string?-indexes r) 1)))
      (is (and (= 1 (count (:int64-indexes r))) (contains? (:int64-indexes r) 2))))))

(deftest library-precomputes-one-record-per-fn-key
  (let [lib (d/library {:key :test-lib :fndefs sample-fndefs})]
    (testing "every fn-key gets a record"
      (is (= 3 (count (:fns lib))))
      (is (every? #(contains? (:fns lib) %) (keys sample-fndefs))))
    (testing "the record carries the computed ccall types, not the raw fn-def"
      (let [r (get-in lib [:fns :lib_make])]
        (is (= :number (:ccall-rettype r)))
        (is (= [:number :string] (:ccall-argtypes r)))
        (is (= "lib_make" (:c-name r)))))
    (testing "the original fn-def stays reachable"
      (is (= (:lib_count sample-fndefs) (:fn-def (get-in lib [:fns :lib_count])))))
    (testing "the key is carried for pool and wasm-context lookups"
      (is (= :test-lib (:key lib))))))

#?(:clj
   (deftest jvm-pointer-postprocess-maps-a-zero-address-to-nil
     (is (nil? (d/jvm-rettype-postprocess :pointer 0))
         "a null pointer return is nil, not a zero-address TrackablePointer")
     (is (some? (d/jvm-rettype-postprocess :pointer 4096))
         "a live address still wraps")))

#?(:clj
   (deftest jvm-postprocess-treats-pointer?-exactly-like-pointer
     ;; The CLJS backend nils a zero for both spellings, so a missing :pointer?
     ;; arm here would make the backends disagree.
     (testing "a null address is nil under either spelling"
       (is (nil? (d/jvm-rettype-postprocess :pointer? 0))))
     (testing "a live address wraps under either spelling"
       (is (some? (d/jvm-rettype-postprocess :pointer? 4096)))
       (is (= (class (d/jvm-rettype-postprocess :pointer 4096))
              (class (d/jvm-rettype-postprocess :pointer? 4096)))
           "both spellings produce the same wrapper type"))))

(deftest validate-fn-def!-names-every-supported-type
  ;; supported-types-msg is a literal, since (str :pointer) differs between
  ;; the JVM and squint. This test keeps it in step with the set.
  (let [msg (try
              (d/library {:key :msg-lib
                          :fndefs {:lib_bad {:rettype :int128 :argtypes []}}})
              nil
              (catch #?(:clj Exception :cljs :default) e
                #?(:clj (.getMessage e) :cljs (.-message e))))]
    (is (some? msg) "an unsupported rettype has to throw for this to mean anything")
    ;; Match whole tokens, since ":string" is a prefix of ":string?".
    (let [tokens (set (string/split msg #"[ ,]+"))]
      (is (contains? tokens ":pointer") "the token split has to produce bare types")
      (doseq [t d/supported-types]
        (is (contains? tokens (str ":" (name t)))
            (str "the rejection message omits " (name t)))))))

#?(:clj
   (deftest call!-selects-the-backend-from-the-impl-atom-per-call
     (let [impl  (atom :ffi)
           calls (atom [])
           lib   (d/library {:key :test-lib
                             :fndefs sample-fndefs
                             :impl-atom impl
                             :ffi-impl-ns 'net.willcohen.native.dispatch-test})]
       (with-redefs [net.willcohen.native.platform/call-native-fn
                     (fn [_ns fn-key args] (swap! calls conj [:ffi fn-key args]) :ffi-result)]
         (is (= :ffi-result (d/call! lib :lib_count [7])))
         (is (= [[:ffi :lib_count [7]]] @calls) "routed to the FFI leaf"))
       (testing "flipping the atom flips the backend with no rebuild"
         (reset! impl :graal)
         (with-redefs [net.willcohen.native.dispatch/jvm-graal-call
                       (fn [& _] 99)]
           ;; Postprocess passes an :int32 through, so the stub's value returns.
           (is (= 99 (d/call! lib :lib_count [7]))
               "same library value now routes to graal")))
       (testing "the FFI backend gives a C string for a :string? argument, and nil for nil"
         (reset! impl :ffi)
         (reset! calls [])
         (let [sq-lib (d/library {:key :sq-lib
                                  :fndefs {:lib_opt {:rettype :void
                                                     :argtypes [[:key :string?] [:n :int32]]}}
                                  :impl-atom impl
                                  :ffi-impl-ns 'net.willcohen.native.dispatch-test})]
           ;; The C string lives until the call returns, so read it in the stub.
           (with-redefs [net.willcohen.native.platform/call-native-fn
                         (fn [_ns fn-key [s n]]
                           (swap! calls conj [fn-key [(some-> s dt-ffi/c->string) n]])
                           nil)]
             (d/call! sq-lib :lib_opt ["abc" 1])
             (d/call! sq-lib :lib_opt [nil 2]))
           (let [[[_ [c-str n1]] [_ [nil-arg n2]]] @calls]
             (is (= "abc" c-str))
             (is (nil? nil-arg))
             (is (= [1 2] [n1 n2]) "the other arguments pass unchanged"))))
       (testing "an unknown fn-key throws rather than deriving a record per call"
         (is (thrown? clojure.lang.ExceptionInfo
                      (d/call! lib :lib_nonexistent [])))))))

#?(:cljs
   (deftest ^:async call!-rejects-an-unknown-fn-key
     ;; call! is ^:async, so squint turns the throw into a rejected promise. A
     ;; try/catch with no await sees nothing.
     (let [lib     (d/library {:key :cljs-unknown-lib :fndefs sample-fndefs})
           outcome (await (-> (d/call! lib :lib_nonexistent [])
                              (.then (fn [_] "resolved"))
                              (.catch (fn [e] (str (.-message e))))))]
       (is (not= "resolved" outcome)
           "an unknown fn-key must not resolve")
       (is (.includes outcome "Unknown fn-key")
           "the rejection names the defect"))))

(deftest pointer-returns-normalize-a-raw-zero-to-nil
  (testing "a null pointer return is nil on every backend"
    (is (nil? (d/normalize-null-pointer :pointer 0)))
    (is (nil? (d/normalize-null-pointer :pointer? 0))))
  (testing "a live address passes through"
    (is (= 4096 (d/normalize-null-pointer :pointer 4096))))
  (testing "only a pointer rettype normalizes"
    (is (= 0 (d/normalize-null-pointer :int32 0))
        "a zero int32 return is a real value, not a null pointer")
    (is (= 0 (d/normalize-null-pointer :size-t 0)))
    (is (= "" (d/normalize-null-pointer :string "")))))

(deftest ^:async result-check-hook-fires-through-check-result
  ;; check-result reads the hook from the library value and calls it with
  ;; [library fn-key fn-def opts result].
  (let [seen (atom nil)
        lib  (d/library {:key :dispatch-check-lib
                         :fndefs sample-fndefs
                         :hooks {:result-check
                                 (fn [l fn-key _fn-def _opts result]
                                   (reset! seen [(:key l) fn-key])
                                   (* 10 result))}})
        r #?(:clj  (d/check-result lib :some-fn {} {} 21)
             :cljs (await (d/check-result lib :some-fn {} {} 21)))]
    (is (= 210 r) "check-result runs the library's check on the result arg")
    (is (= [:dispatch-check-lib :some-fn] @seen)
        "the check receives the library VALUE, not a key")))

(deftest ^:async check-result-passes-through-when-the-library-has-no-check
  (let [lib (d/library {:key :dispatch-nocheck-lib :fndefs sample-fndefs})
        r #?(:clj  (d/check-result lib :some-fn {} {} 21)
             :cljs (await (d/check-result lib :some-fn {} {} 21)))]
    (is (= 21 r) "no hook means the result passes through")))

#?(:cljs
   (deftest ^:async call!-on-cljs-sends-an-int64-argument-as-a-bigint
     ;; A WASM_BIGINT module rejects a JS number for an i64 parameter.
     (let [seen (atom nil)
           fake #js {:worker (fn [_idx]
                               (js-obj "i64-lib"
                                       #js {:ccall (fn [_fn _ret _types args]
                                                     (reset! seen args)
                                                     7)}))}
           lib  (d/library {:key :i64-lib
                            :fndefs {:lib_set64 {:rettype :int32
                                                 :argtypes [[:v :int64] [:n :int32]]}}})
           r    (await (d/call! lib :lib_set64 [3000000000 1] #js {:pool fake}))]
       (is (= 7 r))
       (is (= (js/BigInt "3000000000") (aget @seen 0)) "the :int64 argument is a BigInt")
       (is (= 1 (aget @seen 1)) "an :int32 argument stays a number"))))

#?(:cljs
   (deftest ^:async call!-on-cljs-runs-the-hooks-in-order
     (let [seen (atom nil)
           fake #js {:worker (fn [_idx]
                               (js-obj "hook-lib" #js {:ccall (fn [& xs] (reset! seen (vec xs)) 5)}))}
           lib  (d/library
                 {:key :hook-lib
                  :fndefs {:lib_iso {:rettype :int32 :argtypes [[:a :int32]] :isolate-context? true}}
                  :hooks {:extras-builder (fn [_fn-def args]
                                            {:args (mapv inc args)
                                             :extras {:coords "c" :none nil}
                                             :on-result (fn [r] (* 10 r))})
                          :context-isolator (fn [{:keys [args]}]
                                              (js/Promise.resolve {:args (mapv #(* 2 %) args) :clone 9}))
                          :result-wrapper (fn [{:keys [result isolator-result]}]
                                            [result (:clone isolator-result)])}})
           r    (await (d/call! lib :lib_iso [1] #js {:pool fake}))]
       (is (= [50 9] r) "on-result, then the result-wrapper with the isolator map")
       (is (= [4] (vec (nth @seen 3))) "the builder args, then the isolator args")
       (is (= "c" (.-coords (nth @seen 4))) "an extra goes in the fifth arg")
       (is (not (.hasOwnProperty (nth @seen 4) "none")) "a nil extra does not"))))

#?(:cljs
   (deftest ^:async call!-on-cljs-rejects-an-evicted-handle-before-any-call
     (let [calls (atom 0)
           fake  #js {:worker (fn [_idx]
                                (js-obj "ev-lib" #js {:ccall (fn [& _] (swap! calls inc) 1)}))}
           lib   (d/library {:key :ev-lib :fndefs {:lib_use {:rettype :int32 :argtypes [[:h :pointer]]}}})
           owner #js {}]
       (pool/register-library-context! :ev-lib {:min-age-ms 0})
       (pool/register-handle! :ev-lib "h1" 0 (fn [] (js/Promise.resolve nil)) owner)
       (pool/evict-oldest! :ev-lib)
       (let [err (await (-> (d/call! lib :lib_use [#js {:ctx_id "h1" :ptr 8}] #js {:pool fake})
                            (.then (fn [_] nil))
                            (.catch (fn [e] (.-message e)))))]
         (is (and err (.includes err "evicted")))
         (is (= 0 @calls))))))

#?(:cljs (tr/run-tests-and-exit! "net.willcohen.native.dispatch-test"))
