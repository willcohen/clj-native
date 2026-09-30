;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.callbacks
  "JVM helper for dt-ffi upcall callbacks. Under the :jdk backend, a callback
   compiles to a Panama upcall stub, and the native library holds a raw
   pointer to it. The interface instance must stay reachable while the
   library keeps that pointer.

   define-foreign-interface loads a new class on each call, so define an
   interface once, in a delay or a defonce, and instantiate it again with
   register-callback!."
  (:require [net.willcohen.native.platform :as platform]
            [tech.v3.datatype.ffi :as dt-ffi]))

(defn define-callback-interface
  "A dt-ffi foreign interface for an upcall, with return type `rettype` and
   the dt-ffi type keywords `argtypes`, which can be empty."
  [rettype argtypes]
  (platform/call-with-scratch-compile-path
   #(dt-ffi/define-foreign-interface rettype argtypes)))

(defn register-callback!
  "Instantiate `iface` around `ifn`, and convert the instance to a C-callable
   Pointer. Returns {:ptr <Pointer> :inst <instance>}. Keep :inst reachable
   for as long as the native side holds :ptr, or the pointer dangles."
  [iface ifn]
  (let [inst (dt-ffi/instantiate-foreign-interface iface ifn)
        cptr (dt-ffi/foreign-interface-instance->c iface inst)]
    {:ptr cptr :inst inst}))
