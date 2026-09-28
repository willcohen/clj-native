;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.platform-state
  "Platform-selection state of a consumer library, held in two atoms:
     impl-atom   The chosen impl keyword, nil before init. The JVM uses :ffi
                 or :graal. cljs uses :node, :browser or :unknown.
     force-atom  When true, JVM init uses GraalVM even if FFI works.

   The JVM keywords name a mechanism that try-init! picks at run time. The
   cljs keywords name a host that the consumer sets, because one JS backend
   serves every host. Keep the two sets apart.

   Define both atoms with defonce. dispatch/library holds impl-atom by
   identity, so after a REPL reload of a def the library and these
   predicates read different atoms, with no error.")

(defn ffi?
  [impl-atom]
  (= :ffi @impl-atom))

(defn graal?
  [impl-atom]
  (= :graal @impl-atom))

(defn node?
  [impl-atom]
  (= :node @impl-atom))

(defn force-graal!
  "Force GraalVM, and clear impl-atom so the next init runs again."
  [impl-atom force-atom]
  (reset! force-atom true)
  (reset! impl-atom nil))

(defn force-ffi!
  "Stop forcing GraalVM, and clear impl-atom. The next init tries FFI
   first, with GraalVM as the fallback."
  [impl-atom force-atom]
  (reset! force-atom false)
  (reset! impl-atom nil))

(defn toggle-graal!
  "Flip force-atom, and clear impl-atom."
  [impl-atom force-atom]
  (swap! force-atom not)
  (reset! impl-atom nil))

(defn null-ptr?
  "True when `p` is NULL: nil or a numeric 0, on each runtime. A heap read
   gives 0 for NULL. Use it in place of nil?, which misses that 0 and can
   make an iterator loop run forever."
  [p]
  (or (nil? p) (and (number? p) (zero? p))))

#?(:clj
   (defn try-init!
     "Run the zero-argument ffi-fn, and on a Throwable run graal-fn. When
      @force-atom is true, run graal-fn only. Stores the chosen keyword in
      impl-atom and returns it.

      log? prints the chosen path. A fallback always prints its exception,
      because a silent run on GraalVM is hard to diagnose."
     [impl-atom force-atom log? ffi-fn graal-fn]
     (let [chosen
           (if @force-atom
             (do (when log? (println "Forcing GraalVM implementation."))
                 (graal-fn)
                 :graal)
             (try
               (when log? (println "Attempting FFI implementation."))
               (ffi-fn)
               :ffi
               (catch Throwable e
                 (println "FFI initialization failed, falling back to GraalVM:")
                 (.printStackTrace e)
                 (graal-fn)
                 :graal)))]
       (reset! impl-atom chosen)
       chosen)))
