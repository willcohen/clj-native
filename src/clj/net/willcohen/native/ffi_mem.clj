;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.ffi-mem
  "Native-memory primitives for the dt-ffi :jdk backend of consumer libraries:
   scalar reads and writes at raw addresses, byte copies, and C string and
   pointer arrays. They use dtype native-buffer and sun.misc.Unsafe, so a
   consumer calls them from a #?(:clj) branch."
  (:require [tech.v3.datatype :as dt]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.ffi.ptr-value :as dt-ptr]
            [tech.v3.datatype.native-buffer :as dt-nb]))

(set! *warn-on-reflection* true)

(defn ptr-addr
  "The raw address of a dt-ffi Pointer or a NativeBuffer. Throws on nil."
  ^long [p]
  (dt-ptr/ptr-value p))

(defn rd-i32 ^long [addr] (long (.getInt ^sun.misc.Unsafe (dt-nb/unsafe) (long addr))))
(defn rd-f64 ^double [addr] (.getDouble ^sun.misc.Unsafe (dt-nb/unsafe) (long addr)))
(defn rd-addr ^long [addr] (.getLong ^sun.misc.Unsafe (dt-nb/unsafe) (long addr)))

(defn rd-cstr
  "Read the char* stored at `addr` into a String, or nil for NULL."
  [addr]
  (let [p (rd-addr addr)]
    (when-not (zero? p) (dt-ffi/c->string (dt-ffi/->pointer p)))))

(defn put-i32! [addr off v]
  (.putInt ^sun.misc.Unsafe (dt-nb/unsafe) (+ (long addr) (long off)) (int v)))

(defn put-i64! [addr off v]
  (.putLong ^sun.misc.Unsafe (dt-nb/unsafe) (+ (long addr) (long off)) (long v)))

(defn put-byte! [addr off v]
  (.putByte ^sun.misc.Unsafe (dt-nb/unsafe) (+ (long addr) (long off)) (byte v)))

(def ^{:doc "Write a 64-bit pointer. Same as put-i64!."}
  put-ptr! put-i64!)

(defn copy-bytes!
  "Copy byte[] `src` to raw address `addr`. Returns the byte count."
  ^long [addr ^bytes src]
  (let [n (alength src)]
    (when (pos? n)
      (dt/copy! src (dt-nb/wrap-address (long addr) n)))
    n))

(defn strings->c-array
  "Build a NULL-terminated `char* const*` of `strs` as one NativeBuffer:
   pointer slots, then the UTF-8 payloads. Keep it reachable across the
   native call."
  [strs]
  (let [byte-arrs (mapv #(.getBytes ^String % "UTF-8") strs)
        n (count byte-arrs)
        ptr-bytes (* 8 (inc n))
        total (+ ptr-bytes (reduce + 0 (map #(inc (alength ^bytes %)) byte-arrs)))
        ;; malloc zeroes the block, which gives the NULL slot and the NUL
        ;; bytes, so no :uninitialized? true.
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
  "Pack the addresses of `ptrs` (Pointers or NativeBuffers) into an int64
   NativeBuffer of at least one slot. Keep it reachable across the native
   call."
  [ptrs]
  (let [n (count ptrs)
        buf (dt-nb/malloc (* 8 (max 1 n)) {:datatype :int8})
        base (dt-ptr/ptr-value buf)
        u ^sun.misc.Unsafe (dt-nb/unsafe)]
    (dotimes [i n]
      (.putLong u (+ base (* i 8)) (dt-ptr/ptr-value (nth ptrs i))))
    buf))

(defn alloc-cstring
  "Copy `s` as NUL-terminated UTF-8 into memory from `alloc-fn`, a fn from
   byte count to raw address. Returns the address, or 0 for nil `s`. The
   caller owns the allocation. `alloc-fn` need not zero the block."
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
  "Decode the NULL-terminated `char* const*` at `base-addr` into a vector of
   strings. Address 0 gives []."
  [base-addr]
  (let [base (long base-addr)]
    (if (zero? base)
      []
      (into [] (comp (map #(rd-cstr (+ base (* 8 (long %))))) (take-while some?)) (range)))))
