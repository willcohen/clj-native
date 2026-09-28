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
            #?(:clj  [net.willcohen.native.dispatch :as d]
               :cljs ["ffi-wasm/dispatch" :as d])
            #?(:clj [net.willcohen.native.platform])
            #?(:clj [tech.v3.datatype.ffi :as dt-ffi])
            #?(:cljs ["ffi-wasm/pool" :as pool])
            #?(:cljs ["ffi-wasm/test-runner" :as tr])))

(def sample-fndefs
  {:lib_make  {:rettype :pointer :argtypes [[:ctx :pointer] [:name :string]]}
   :lib_count {:rettype :int32   :argtypes [[:ctx :pointer]]}
   :lib_free  {:rettype :void    :argtypes [[:handle :pointer]]}})

;; squint compiles :number to the string "number", so one assertion holds on
;; both platforms.
(deftest argtype->ccall-type-maps-strings-and-numbers
  (is (= [:string :string :number :number]
         (mapv d/argtype->ccall-type [:string :string? :int64 :pointer?]))))

(deftest library-validates-fndefs-types-at-build-time
  ;; argtype->ccall-type maps an unknown type to :number with no error, so
  ;; `library` must reject it.
  (testing "an unknown rettype throws at assembly"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (d/library {:key :bad-lib
                             :fndefs {:lib_bad {:rettype :int128
                                                :argtypes [[:x :pointer]]}}}))))
  (testing "an unknown argtype throws at assembly"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (d/library {:key :bad-lib
                             :fndefs {:lib_bad {:rettype :void
                                                :argtypes [[:x :quaternion]]}}})))))

#?(:clj
   (deftest jvm-postprocess-treats-pointer?-exactly-like-pointer
     ;; The CLJS backend nils a zero for both spellings, so a missing :pointer?
     ;; arm here would make the backends disagree.
     (doseq [t [:pointer :pointer?]]
       (is (nil? (d/jvm-rettype-postprocess t 0)) (str t " of NULL"))
       (is (= 4096 (:address (d/jvm-rettype-postprocess t 4096)))
           (str t " of a live address")))))

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
         (is (= [[:ffi :lib_count [7]]] @calls) "routed to the FFI backend"))
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
       (testing "an unknown fn-key throws"
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
        "a zero int32 return stays 0")))

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
             :cljs (await (d/check-result lib :some-fn {} {} 21)))
        no-hook (d/library {:key :dispatch-nocheck-lib :fndefs sample-fndefs})
        r-no-hook #?(:clj  (d/check-result no-hook :some-fn {} {} 21)
                     :cljs (await (d/check-result no-hook :some-fn {} {} 21)))]
    (is (= 210 r) "check-result runs the library's check on the result arg")
    (is (= [:dispatch-check-lib :some-fn] @seen)
        "the check receives the library value")
    (is (= 21 r-no-hook) "no hook means the result passes through")))

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
