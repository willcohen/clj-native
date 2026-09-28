;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.pool-stub-test
  "Pins the JVM stub block of pool.cljc against the CLJS surface it stands in
   for. A missing stub goes unseen in use, since every consumer call site sits
   inside #?(:cljs ...)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [net.willcohen.native.pool])
  (:import [java.io PushbackReader]))

(def ^:private pool-source "net/willcohen/native/pool.cljc")

(def ^:private pool-ns 'net.willcohen.native.pool)

;; A no-op on the JVM, so a consumer can call it with no platform branch.
(def ^:private non-throwing 'set-log-config!)

(defn- read-cljs-forms
  "Read every top-level form of `resource-path` with the :cljs branch selected,
   as squint sees it. An unknown tag such as #js passes its value through."
  [resource-path]
  (with-open [rdr (PushbackReader. (io/reader (io/resource resource-path)))]
    (let [eof  (Object.)
          opts {:read-cond :allow :features #{:cljs} :eof eof}]
      (binding [*default-data-reader-fn* (fn [_tag v] v)
                *read-eval* false]
        (loop [acc []]
          (let [form (read opts rdr)]
            (if (identical? form eof)
              acc
              (recur (conj acc form)))))))))

(defn- public-defn?
  [form]
  (and (seq? form)
       (= 'defn (first form))
       (symbol? (second form))
       (not (:private (meta (second form))))))

(defn- arg-vectors
  "The argument vectors of a defn form, for a single or a multiple arity."
  [form]
  (let [body (drop 2 form)
        body (if (string? (first body)) (rest body) body)
        body (if (map? (first body)) (rest body) body)]
    (if (vector? (first body))
      [(first body)]
      (into [] (comp (filter #(and (seq? %) (vector? (first %))))
                     (map first))
            body))))

(defn- arities
  "The argument count of each vector, as a set. A variadic vector gives
   :variadic in place of a count."
  [arg-vecs]
  (into #{} (map (fn [v] (if (some #{'&} v) :variadic (count v)))) arg-vecs))

(def ^:private cljs-surface
  (delay
    (into {}
          (comp (filter public-defn?)
                (map (fn [form] [(second form) (arities (arg-vectors form))])))
          (read-cljs-forms pool-source))))

(def ^:private jvm-surface
  (delay
    (into {}
          (map (fn [[sym v]]
                 [sym (into #{} (map (fn [al] (if (some #{'&} al) :variadic (count al))))
                            (:arglists (meta v)))]))
          (ns-publics pool-ns))))

(deftest cljs-surface-read-is-not-vacuous
  ;; An empty read would make every other assertion pass.
  (testing "the :cljs read finds a real surface"
    (is (< 25 (count @cljs-surface))
        "expected the CLJS branch to define more than 25 public fns"))
  (testing "the JVM namespace exposes a stub surface"
    (is (< 25 (count @jvm-surface)))))

(deftest every-cljs-public-fn-has-a-jvm-stub
  (let [missing (sort (remove (set (keys @jvm-surface)) (keys @cljs-surface)))]
    (is (empty? missing)
        (str "CLJS public fns with no JVM stub: " (vec missing)))))

(deftest no-jvm-stub-outlives-its-cljs-fn
  (let [orphaned (sort (remove (set (keys @cljs-surface)) (keys @jvm-surface)))]
    (is (empty? orphaned)
        (str "JVM stubs naming a fn the CLJS branch does not define: "
             (vec orphaned)))))

(deftest stub-arities-match-the-cljs-arities
  (doseq [[sym cljs-arities] (sort-by key @cljs-surface)
          :let [jvm-arities (get @jvm-surface sym)]
          :when jvm-arities]
    (testing (str sym)
      (is (= cljs-arities jvm-arities)
          (str sym ": CLJS has " (sort-by str cljs-arities)
               ", the JVM stub has " (sort-by str jvm-arities))))))

(deftest every-stub-throws-with-its-own-name
  ;; A copy-pasted stub still throws, under a neighbor's name, so only the
  ;; ex-data name catches it.
  (doseq [[sym stub-arities] (sort-by key @jvm-surface)
          :when (not= non-throwing sym)
          arity stub-arities
          :when (integer? arity)]
    (testing (str sym "/" arity)
      (let [f (ns-resolve pool-ns sym)
            e (try (apply f (repeat arity nil))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (some? e) (str sym " did not throw"))
        (when e
          (is (= sym (:fn (ex-data e)))
              (str sym " threw under the name " (:fn (ex-data e)))))))))

(deftest the-one-non-throwing-stub-stays-a-no-op
  (testing "set-log-config! does nothing on the JVM, so a consumer needs no
            platform branch"
    (is (nil? (net.willcohen.native.pool/set-log-config! {:level :debug})))
    (is (nil? (net.willcohen.native.pool/set-log-config! nil)))))
