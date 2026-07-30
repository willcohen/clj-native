;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns hooks.native.generated
  "Shared clj-kondo hook support for fndefs-driven generated functions.

   Every consumer builds its public surface the same way: walk an fndefs map
   and define one fn for each entry. Both mechanisms clj-native supports for
   that -- a runtime intern loop, and dt-ffi's define-library-functions --
   are invisible to static analysis, so a caller of a generated fn reads as
   `Unresolved var` and real dead code hides in that noise.

   A consumer keeps a copy of its own fndefs under .clj-kondo/hooks, because a
   hook cannot read a var out of the project it lints. It then calls
   `defn-nodes` with that copy. The data stays per-consumer; this logic does
   not."
  (:require [clj-kondo.hooks-api :as api]
            [clojure.string :as string]))

(defn defn-node
  "Build a defn node for one generated fn.

   `arities` is a vector of argument-name vectors. The default gives the
   0-and-1-arity shape that a fndefs-driven public fn has. An arity that the
   hook omits reads as a false invalid-arity at every call site.

   Each argument list is written with no body. A body of `nil` tells
   clj-kondo that the function always returns nil. Correct code then reads
   as an error: `(inc (proj_context_errno))` reports \"Expected: number,
   received: nil\". With no body, clj-kondo has no opinion about the return
   value. That is the truth, because the real function is made at run time
   and returns what the native call returns. clj-kondo still checks the
   number of arguments."
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
  "Build a `do` node holding one defn for each entry of `fndefs`.

   `fn-key->name` turns a fndefs key into the public symbol. `fn-key->doc` is
   optional and may return nil."
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

   Mirrors net.willcohen.native.macros/camel-name->clj-name, and returns a
   string where that one returns a symbol. A hook runs in SCI and cannot call
   into the project it lints, so the algorithm exists twice; it lives here
   rather than in each consumer's hook so there is one copy per side, not one
   per library.

   Two copies of the rule that names every generated fn is a drift risk, so
   generated_hook_test lifts this fn out of the file and asserts it agrees
   with the macros one across a shared corpus. Change one copy and that test
   fails."
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
  "Build a `declare` node naming every generated fn.

   Use this where the arity is not uniform, as with dt-ffi's
   define-library-functions, whose fns take the C signature's arity. A declare
   resolves the var without asserting an arity."
  [fndefs fn-key->name]
  (api/list-node
   (cons (api/token-node 'declare)
         (for [fn-key (keys fndefs)]
           (api/token-node (symbol (fn-key->name fn-key)))))))
