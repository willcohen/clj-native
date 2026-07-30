;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#?(:clj
   (ns net.willcohen.native.macros
     "Surface-generation helpers for libraries that bind a C library. Those
      libraries use the clj-native dispatch infrastructure.

      A consumer keeps its own macro, because only the consumer knows the
      shape of one wrapper fn. This namespace supplies the parts that do
      not differ:
        - The two name mappings
        - The argument symbols of a fndefs entry
        - The walk over fndefs
        - The JVM load-time intern loop

      The .cljc extension lets CLJS consumers `:require` this namespace
      directly. squint 0.11.187 and later auto-load .cljc macros.

      The JVM polyglot lock macro lives next to the Context that it locks.
      Refer to net.willcohen.native.graal-wasm/with-graal-lock."
     (:require [clojure.string :as string]))
   :cljs
   (ns net.willcohen.native.macros
     "Surface-generation helpers for libraries that bind a C library. Refer
      to the JVM namespace docstring for context. This branch drops
      intern-library-fns!, because a CLJS consumer macro emits its surface at
      compile time. It adds underscore->camelCase, for the aliases that JS
      callers want."
     (:require [clojure.string :as string])))

(defn c-name->clj-name
  "Convert a C-style underscore name keyword to a hyphenated symbol. This
   function is cross-platform, and it does string manipulation only. It is
   public, so consumer wrappers and CLJS-side wrapper generators can share
   the canonical mapping. No consumer must write that mapping again.

   This mapping suits a library with C names that are already lower-case
   with underscores. For a mixed-case C API, use camel-name->clj-name."
  [c-fn-keyword]
  (symbol (string/replace (name c-fn-keyword) "_" "-")))

(defn- upper-char?
  [ch]
  (and (not= ch (string/lower-case ch)) (= ch (string/upper-case ch))))

(defn- lower-char?
  [ch]
  (and (not= ch (string/upper-case ch)) (= ch (string/lower-case ch))))

(defn- word-start?
  "Returns true when the upper-case character at `i` opens a new word.

   The character opens a word when the character before it is not
   upper-case. It also opens a word when it is the last capital of a run,
   and a lower-case letter comes after that run. The second test prevents
   the conversion of GDALGetDriverCount into g-d-a-l-get-driver-count."
  [s i]
  (let [prev (subs s (dec i) i)
        nxt  (when (< (inc i) (count s)) (subs s (inc i) (+ i 2)))]
    (or (not (upper-char? prev))
        (and (some? nxt) (lower-char? nxt)))))

(defn camel-name->clj-name
  "Convert a mixed-case C name keyword to a hyphenated symbol.

   This function puts a hyphen before each upper-case letter that opens a
   word. It then lower-cases the result. Thus GDALGetDriverCount becomes
   gdal-get-driver-count, and OGR_L_GetName becomes ogr-l-get-name. An
   upper-case run stays in one piece, which keeps an acronym such as
   PROJJSON readable. An underscore becomes a hyphen, and it never doubles
   one.

   This function is cross-platform, and it uses no regular expression.
   That is deliberate. The JVM replaces every match, while JS replaces
   only the first match. JS replaces every match only with a global
   pattern. Thus a regex here would behave differently in the two lanes."
  [c-fn-keyword]
  (let [s (name c-fn-keyword)
        n (count s)]
    (loop [i 0, out "", pending-sep? false]
      (if (>= i n)
        (symbol out)
        (let [ch (subs s i (inc i))]
          (if (= ch "_")
            (recur (inc i) out (not= out ""))
            (let [sep? (or pending-sep?
                           (and (not= out "")
                                (upper-char? ch)
                                (word-start? s i)))]
              (recur (inc i)
                     (str out (if sep? "-" "") (string/lower-case ch))
                     false))))))))

(defn fn-def-arg-syms
  "Return the argument symbols of a fndefs entry, in C signature order.

   Each :argtypes entry is [arg-name type]. The name becomes one
   parameter symbol of a generated wrapper. A name that repeats inside
   one signature gets a numeric suffix, because two identical parameter
   symbols would shadow."
  [fn-def]
  (first
   (reduce (fn [[syms seen] [arg-name _]]
             (let [base (name arg-name)
                   n    (get seen base 0)
                   sym  (symbol (if (zero? n) base (str base "-" (inc n))))]
               [(conj syms sym) (assoc seen base (inc n))]))
           [[] {}]
           (:argtypes fn-def))))

(defn library-fns-form
  "Build a `do` form that defines the public wrapper fns of a library.

   `name-fn` maps a fndefs key to the public symbol. It can return nil, and
   then library-fns-form omits that entry from the surface. `emit-fn` gets
   that symbol with the key and the fn-def, and returns one whole defn
   form. `alias-name-fn` and `alias-emit-fn` add an optional second walk.
   That second walk is how a library gives its fns a second spelling.

   name-fn and emit-fn run when the consumer macro expands. Thus a consumer
   passes plain functions, and keeps its own syntax-quoted templates."
  [fndefs {:keys [name-fn emit-fn alias-name-fn alias-emit-fn]}]
  (let [walk (fn [nf ef]
               (when (and nf ef)
                 (keep (fn [[fn-key fn-def]]
                         (when-let [fn-name (nf fn-key)]
                           (ef fn-name fn-key fn-def)))
                       fndefs)))]
    (cons 'do (concat (walk (or name-fn c-name->clj-name) emit-fn)
                      (walk alias-name-fn alias-emit-fn)))))

#?(:clj
   (defn intern-library-fns!
     "Intern one public wrapper fn for each fndefs entry into `ns-sym`.

      The JVM reader cannot see the fndefs shape while the consumer macro
      expands. Thus intern-library-fns! builds the surface when the
      namespace loads. `name-fn` maps a fndefs key to the public symbol.
      It can return nil, and then intern-library-fns! omits that entry.
      `make-fn` gets the key and the fn-def, and returns the fn to intern.

      Call intern-library-fns! from inside the consumer macro only. Never
      call it directly. A direct call gives clj-kondo nothing to expand.
      Every caller of a generated fn then reads as an unresolved var, and
      that noise hides real dead code."
     [ns-sym fndefs name-fn make-fn]
     (doseq [[fn-key fn-def] fndefs]
       (when-let [fn-name (name-fn fn-key)]
         (intern ns-sym fn-name (make-fn fn-key fn-def))))
     nil))

#?(:cljs
   (defn underscore->camelCase
     "Convert a snake_case string to camelCase. This function is JS-side
      only. Consumer macros call it when they emit camelCase aliases for
      JavaScript callers. It lives here and not in each consumer, so all
      libraries share the canonical mapping."
     [s]
     (let [parts (.split s "_")]
       (apply str (first parts)
              (map #(str (.toUpperCase (.substring % 0 1)) (.substring % 1))
                   (rest parts))))))

