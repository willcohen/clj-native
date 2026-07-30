;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.callbacks
  "JVM-only helper for dt-ffi upcall callbacks. This namespace registers a
   callback and keeps it reachable, so the garbage collector does not collect
   it. Under the :jdk backend, a callback compiles to a Panama upcall stub. The
   native library holds a raw function pointer to that stub. Thus the interface
   instance must stay reachable while the library keeps the pointer.

   Consumers keep their own callback bodies, C signatures, argument order, and
   native registration calls. This namespace owns only the generic shape of
   define, instantiate, convert to C, and retain.

   This namespace splits interface definition from instantiation on purpose.
   Under the :jdk backend, `define-foreign-interface` generates and loads a
   fresh class for each call, with a gensym classname. Thus a consumer that can
   re-register the same callback must define the interface one time only. Cache
   the interface in a delay or a defonce, and re-instantiate it with
   register-callback!. An idempotent setup function is one example of such a
   consumer."
  (:require [tech.v3.datatype.ffi :as dt-ffi]))

(defn define-callback-interface
  "Define a dt-ffi foreign interface for an upcall. `rettype` is the return
   type. `argtypes` is a seq of dt-ffi type keywords, and it can be empty. Cache
   the result in a delay or a defonce. Use the same interface again for each
   subsequent registration."
  [rettype argtypes]
  (dt-ffi/define-foreign-interface rettype argtypes))

(defn register-callback!
  "Instantiate `iface` around `ifn`. `iface` comes from
   define-callback-interface. Then convert the instance to a C-callable Pointer.
   Returns {:ptr <Pointer> :inst <instance>}.

   The caller MUST retain the returned map for as long as the native side holds
   :ptr. Retention of the :inst value alone is sufficient. If nothing retains
   the instance, the garbage collector collects the upcall stub. The pointer
   then dangles."
  [iface ifn]
  (let [inst (dt-ffi/instantiate-foreign-interface iface ifn)
        cptr (dt-ffi/foreign-interface-instance->c iface inst)]
    {:ptr cptr :inst inst}))
