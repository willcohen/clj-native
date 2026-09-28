;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns hooks.native.generated
  "clj-kondo hook support for the fns a consumer generates from an fndefs
   map. clj-kondo cannot see an intern loop or dt-ffi
   define-library-functions, so each call reads as `Unresolved var`.

   A hook cannot read a var of the project it lints. A consumer keeps a copy
   of its fndefs under .clj-kondo/hooks and passes it to `defn-nodes`."
  (:require [clj-kondo.hooks-api :as api]
            [clojure.string :as string]))

(defn defn-node
  "A defn node for one generated fn. `arities` is a vector of arg vectors,
   default [] and [opts].

   Each arity has no body. A nil body tells clj-kondo that the fn returns
   nil, and then a correct call such as `(inc (f))` reports a type error."
  ([fn-name] (defn-node fn-name nil [[] ['opts]]))
  ([fn-name docstring arities]
   (api/list-node
    (concat [(api/token-node 'defn)
             (api/token-node (symbol fn-name))]
            (when docstring [(api/string-node docstring)])
            (for [args arities]
              (api/list-node
               (list (api/vector-node (mapv api/token-node args)))))))))

(defn defn-nodes
  "A `do` node with one defn for each entry of `fndefs`. `fn-key->name`
   gives the public name. `fn-key->doc` may return nil."
  ([fndefs fn-key->name] (defn-nodes fndefs fn-key->name nil))
  ([fndefs fn-key->name fn-key->doc]
   (api/list-node
    (cons (api/token-node 'do)
          (for [fn-key (keys fndefs)]
            (defn-node (fn-key->name fn-key)
              (when fn-key->doc (fn-key->doc fn-key))
              [[] ['opts]]))))))

(defn- upper-char? [ch]
  (and (not= ch (string/lower-case ch)) (= ch (string/upper-case ch))))

(defn- lower-char? [ch]
  (and (not= ch (string/upper-case ch)) (= ch (string/lower-case ch))))

(defn camel-name->clj-name
  "Convert a mixed-case C name keyword to the public fn name, as a string.
   A copy of net.willcohen.native.macros/camel-name->clj-name, since a hook
   cannot call the project it lints. generated_hook_test checks that the two
   agree."
  [c-fn-keyword]
  (let [s (name c-fn-keyword)
        n (count s)]
    (loop [i 0, out "", pending-sep? false]
      (if (>= i n)
        out
        (let [ch (subs s i (inc i))]
          (if (= ch "_")
            (recur (inc i) out (not= out ""))
            (let [prev (when (pos? i) (subs s (dec i) i))
                  nxt  (when (< (inc i) n) (subs s (inc i) (+ i 2)))
                  sep? (or pending-sep?
                           (and (not= out "")
                                (upper-char? ch)
                                (or (not (upper-char? prev))
                                    (and (some? nxt) (lower-char? nxt)))))]
              (recur (inc i)
                     (str out (if sep? "-" "") (string/lower-case ch))
                     false))))))))

(defn declare-node
  "One `declare` node naming each generated fn. Use it where arities differ, as
   with dt-ffi define-library-functions: a declare asserts no arity."
  [fndefs fn-key->name]
  (api/list-node
   (cons (api/token-node 'declare)
         (for [fn-key (keys fndefs)]
           (api/token-node (symbol (fn-key->name fn-key)))))))
