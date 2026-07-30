;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.ffi-mem
  "JVM-only native-memory primitives. The Panama dt-ffi FFI backend of each
   consumer library shares these primitives.

   The primitives read and write scalars at raw addresses. They copy byte
   payloads. They build self-contained C string arrays and pointer arrays.
   They also walk null-terminated char** arrays.

   These primitives are Panama-only. There is one memory model at this time:
   the dtype `native-buffer` and `sun.misc.Unsafe` on the :jdk backend. Thus
   the primitives have no branch for each backend.

   They stay JVM-only on purpose. Consumers reach them from the `#?(:clj)`
   branch of their cljc. The cljs and wasm side never sees Unsafe or
   java.lang.foreign."
  (:require [tech.v3.datatype :as dt]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.ffi.ptr-value :as dt-ptr]
            [tech.v3.datatype.native-buffer :as dt-nb]))

(set! *warn-on-reflection* true)

(defn ptr-addr
  "Returns the raw native address (long) of a dt-ffi Pointer or a dtype
   NativeBuffer."
  ^long [p]
  (dt-ptr/ptr-value p))

(defn rd-i32 ^long [addr] (long (.getInt ^sun.misc.Unsafe (dt-nb/unsafe) (long addr))))
(defn rd-f64 ^double [addr] (.getDouble ^sun.misc.Unsafe (dt-nb/unsafe) (long addr)))
(defn rd-addr ^long [addr] (.getLong ^sun.misc.Unsafe (dt-nb/unsafe) (long addr)))

(defn rd-cstr
  "Read the char* at `addr` into a String. That char* can be NULL, and then
   rd-cstr returns nil."
  [addr]
  (let [p (rd-addr addr)]
    (when-not (zero? p) (dt-ffi/c->string (dt-ffi/->pointer p)))))

(defn put-i32! [addr off v]
  (.putInt ^sun.misc.Unsafe (dt-nb/unsafe) (+ (long addr) (long off)) (int v)))

(defn put-i64! [addr off v]
  (.putLong ^sun.misc.Unsafe (dt-nb/unsafe) (+ (long addr) (long off)) (long v)))

(defn put-byte! [addr off v]
  (.putByte ^sun.misc.Unsafe (dt-nb/unsafe) (+ (long addr) (long off)) (byte v)))

(def ^{:doc "Write a 64-bit pointer value. All pointers are 64-bit, thus put-ptr! is put-i64!."}
  put-ptr! put-i64!)

(defn copy-bytes!
  "Copy all of `src`, a byte[], into native memory at raw address `addr`. An
   empty array causes no write. Returns the number of copied bytes."
  ^long [addr ^bytes src]
  (let [n (alength src)]
    (when (pos? n)
      (dt/copy! src (dt-nb/wrap-address (long addr) n)))
    n))

(defn strings->c-array
  "Build a NULL-terminated `char* const*` from `strs` as ONE self-contained
   native buffer. The buffer starts with one pointer slot for each string, and
   a trailing NULL slot. The UTF-8 string payloads come after those slots.
   Each slot holds the address of its own inline bytes.

   strings->c-array never writes the trailing NULL slot, and it never writes
   the NUL byte after each payload. It relies on `dt-nb/malloc` for both,
   because malloc initializes the block to zero. Do not pass
   `:uninitialized? true` here.

   Only the returned buffer must stay reachable for the garbage collector
   across the native call. Returns the NativeBuffer. Pass it wherever a
   caller expects a `char* const*`."
  [strs]
  (let [byte-arrs (mapv #(.getBytes ^String % "UTF-8") strs)
        n (count byte-arrs)
        ptr-bytes (* 8 (inc n))
        total (+ ptr-bytes (reduce + 0 (map #(inc (alength ^bytes %)) byte-arrs)))
        buf (dt-nb/malloc total {:datatype :int8})
        base (dt-ptr/ptr-value buf)
        u ^sun.misc.Unsafe (dt-nb/unsafe)]
    (loop [i 0 off ptr-bytes]
      (when (< i n)
        (let [ba ^bytes (nth byte-arrs i)
              len (alength ba)
              saddr (+ base off)]
          (.putLong u (+ base (* i 8)) saddr)
          (dt/copy! ba (dt-nb/wrap-address saddr len))
          (recur (inc i) (+ off len 1)))))
    buf))

(defn ptrs->native-array
  "Pack the native addresses of `ptrs` into a contiguous int64 array. `ptrs`
   holds dt-ffi Pointers or NativeBuffers. The result is one native buffer
   with at least one slot. Returns the NativeBuffer. Keep it reachable for the
   garbage collector across the native call."
  [ptrs]
  (let [n (count ptrs)
        buf (dt-nb/malloc (* 8 (max 1 n)) {:datatype :int8})
        base (dt-ptr/ptr-value buf)
        u ^sun.misc.Unsafe (dt-nb/unsafe)]
    (dotimes [i n]
      (.putLong u (+ base (* i 8)) (dt-ptr/ptr-value (nth ptrs i))))
    buf))

(defn alloc-cstring
  "Allocate a NUL-terminated UTF-8 copy of `s` with `alloc-fn`, and return its
   address. For a nil `s`, returns 0. `alloc-fn` takes a byte count and
   returns a raw address. One example is a wrapper around the malloc-family
   allocator of the library.

   alloc-cstring writes the terminator itself, and does not rely on the
   allocator to initialize the block to zero.

   The caller that supplies `alloc-fn` owns the allocation. The caller frees
   it, or the native library frees it for a library allocator."
  ^long [alloc-fn s]
  (if (nil? s)
    0
    (let [bytes (.getBytes ^String s "UTF-8")
          n (alength bytes)
          addr (long (alloc-fn (inc n)))]
      (copy-bytes! addr bytes)
      (put-byte! addr n 0)
      addr)))

(defn read-string-array
  "Walk a NULL-terminated `char* const*` at raw address `base-addr`. A
   `base-addr` of 0 gives an empty vector. Returns a vector of the decoded
   strings."
  [base-addr]
  (let [base (long base-addr)]
    (if (zero? base)
      []
      (loop [i 0 acc []]
        (let [p (rd-addr (+ base (* i 8)))]
          (if (zero? p)
            acc
            (recur (inc i) (conj acc (dt-ffi/c->string (dt-ffi/->pointer p))))))))))
