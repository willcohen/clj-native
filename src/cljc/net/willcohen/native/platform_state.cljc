;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.platform-state
  "Predicates and the init flow for the platform-selection state of each
   consumer library. Each consumer owns two atoms:
     impl-atom   The chosen impl keyword. On the JVM this is :ffi or :graal.
                 On cljs this is :node, :browser or :unknown. It is nil
                 before init.
     force-atom  A boolean. When it is true, JVM init forces GraalVM even if
                 FFI works.

   Five keywords for three backends, because the two hosts answer two
   different questions. :ffi and :graal name a MECHANISM, and try-init! below
   picks between them at run time when it catches a Throwable. :node,
   :browser and :unknown name a HOST ENVIRONMENT, and a cljs consumer sets
   the value itself, because one JS backend serves every environment. So the
   JVM discovers its backend, and the JS side declares its surroundings. The
   asymmetry is deliberate. Do not collapse the two sets into one axis.

   The two atoms MUST be defonce. Do not use def. dispatch/library captures
   impl-atom by identity. It keeps that atom for the life of the library
   value, which is itself a defonce.

   A plain def gives a fresh atom each time the namespace reloads. The library
   then reads the previous atom, while the predicates here read the new one.
   Nothing fails. The two only disagree about which backend is in use. A REPL
   reload is the usual path into this state, because a reload is how a
   developer applies a change.")

(defn ffi?
  "Returns true if and only if impl-atom holds :ffi."
  [impl-atom]
  (= :ffi @impl-atom))

(defn graal?
  "Returns true if and only if impl-atom holds :graal."
  [impl-atom]
  (= :graal @impl-atom))

(defn node?
  "Returns true if and only if impl-atom holds :node."
  [impl-atom]
  (= :node @impl-atom))

(defn force-graal!
  "Set force-atom to true. Then clear impl-atom, so a subsequent init
   runs again and chooses GraalVM."
  [impl-atom force-atom]
  (reset! force-atom true)
  (reset! impl-atom nil))

(defn force-ffi!
  "Set force-atom to false. Then clear impl-atom, so a subsequent init
   runs again and chooses FFI. GraalVM stays as the fallback."
  [impl-atom force-atom]
  (reset! force-atom false)
  (reset! impl-atom nil))

(defn toggle-graal!
  "Set force-atom to the opposite of its current value. Then clear
   impl-atom."
  [impl-atom force-atom]
  (swap! force-atom not)
  (reset! impl-atom nil))

;; FFI null-pointer convention. JVM dt-ffi returns nil for NULL. cljs and wasm
;; return 0, and (nil? 0) is false. Thus a JVM (nil? ptr) check loops forever on
;; cljs. One example is a loop over a next-record iterator that returns NULL at
;; the end. Use null-ptr? and some-ptr? on all runtimes.
(defn null-ptr?
  "Returns true if and only if `p` is the null pointer for the current
   runtime. JVM dt-ffi gives nil for NULL. cljs and wasm give 0."
  [p]
  #?(:clj  (nil? p)
     :cljs (or (nil? p) (zero? p))))

(defn some-ptr?
  "The complement of null-ptr?. Returns true if and only if `p` is a
   non-null pointer."
  [p]
  (not (null-ptr? p)))

#?(:clj
   (defn try-init!
     "JVM-only. Run ffi-fn. If ffi-fn throws a Throwable, run graal-fn instead.
      When @force-atom is true, run graal-fn directly. Records the chosen impl
      keyword and returns it.

      ffi-fn and graal-fn are zero-argument bootstrap functions, and try-init!
      discards their return values. When log? is true, try-init! prints the
      chosen path to stdout.

      A fallback always reports itself, whatever log? says. A silent run on the
      slower backend is the failure mode with the highest cost to diagnose. The
      exception is also the only record that try-init! tried the FFI path."
     [impl-atom force-atom log? ffi-fn graal-fn]
     (let [chosen
           (cond
             @force-atom
             (do (when log? (println "Forcing GraalVM implementation."))
                 (graal-fn)
                 :graal)

             :else
             (try
               (when log? (println "Attempting FFI implementation."))
               (ffi-fn)
               :ffi
               (catch Throwable e
                 (println "-------------------- FFI Initialization Failure --------------------")
                 (println "FFI initialization failed, falling back to GraalVM.")
                 (println (str "Top-level exception: " (.getClass e) " - " (.getMessage e)))
                 (when-let [cause (.getCause e)]
                   (println (str "Root cause: " (.getClass cause) " - " (.getMessage cause))))
                 (.printStackTrace e)
                 (println "------------------------------------------------------------------")
                 (graal-fn)
                 :graal)))]
       (reset! impl-atom chosen)
       chosen)))
