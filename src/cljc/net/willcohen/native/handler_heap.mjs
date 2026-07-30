// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Heap helpers for wasm-backed handlers. This module puts emscripten's heap
// views, getValue, setValue, the UTF-8 helpers and the string-array walk into
// one methods object. The consumer spreads that object into the `methods` map
// of its handler. The module binds late through getModule, so the consumer can
// build the object before init() resolves the module.
//
// An offset is an index into the typed-array view. It is not a byte address.
// A caller that holds a byte pointer must divide it by the size of one
// element: ptr / 4 for heap32, ptr / 8 for heapf64. An offset past the end of
// the view causes a RangeError.
//
// There is no heap64 or heapu64 pair. Emscripten emits HEAP64 and HEAPU64 only
// under WASM_BIGINT. Add the pair here if a consumer builds with that flag.
//
// heap*_get returns a typed array, not a plain array. structuredClone keeps
// typed arrays and uses the transferable path, which is faster for bulk binary
// data. A caller that needs a plain array applies Array.from at the call site.
//
// Every method is async, because handler-runtime requires async bodies. The
// work itself is synchronous against the loaded module.

// The methods that have no value to return all answer with this one object.
// Object.freeze makes it safe to share: an ES module is strict mode, so a
// caller that writes to the result gets an error. Without the freeze, that
// write would change the answer that every later call gives.
const ok = Object.freeze({ ok: true });

// A pointer slot is 4 bytes because emcc-link builds every module with
// --target=wasm32-unknown-emscripten. A MEMORY64 build would widen this, and
// read_string_array below is the only place that walks raw slots.
const POINTER_SIZE = 4;

export const heapHelpers = (getModule) => {
  const m = () => {
    const mod = getModule();
    if (!mod) throw new Error('handler-heap: getModule() returned null (called before init?)');
    return mod;
  };

  // [method prefix, emscripten HEAP property]
  const heaps = [
    ['heap8', 'HEAP8'],
    ['heapu8', 'HEAPU8'],
    ['heap16', 'HEAP16'],
    ['heapu16', 'HEAPU16'],
    ['heap32', 'HEAP32'],
    ['heapu32', 'HEAPU32'],
    ['heapf32', 'HEAPF32'],
    ['heapf64', 'HEAPF64'],
  ];

  const out = {
    malloc: async (size) => m()._malloc(size),
    free: async (ptr) => { m()._free(ptr); return ok; },

    get_value: async (ptr, type) => m().getValue(ptr, type),
    set_value: async (ptr, value, type) => { m().setValue(ptr, value, type); return ok; },

    utf8_to_string: async (ptr) => m().UTF8ToString(ptr),
    string_to_utf8: async (str, ptr, maxLength) => {
      m().stringToUTF8(str, ptr, maxLength);
      return ok;
    },
    utf8_byte_length: async (str) => m().lengthBytesUTF8(str),

    // Walk a NUL-terminated `char* const*` into an array of strings. A null
    // list pointer gives []. Reads slots with getValue and '*', matching the
    // JVM twin at graal-wasm/string-array-pointer->strs.
    //
    // The heap-length bound is load-bearing. An out-of-range typed-array read
    // in JS gives undefined, and undefined is not 0, so an unterminated walk
    // never meets its terminator and spins the worker forever. Nothing here
    // allocates, so the heap cannot grow mid-walk and one length read holds.
    read_string_array: async (ptr) => {
      const out = [];
      if (!ptr) return out;
      const mod = m();
      if (ptr % POINTER_SIZE !== 0) {
        throw new Error(
          `handler-heap: read_string_array pointer ${ptr} is not ${POINTER_SIZE}-byte aligned`,
        );
      }
      const limit = mod.HEAPU8.length;
      for (let slot = ptr; ; slot += POINTER_SIZE) {
        if (slot + POINTER_SIZE > limit) {
          throw new Error(
            `handler-heap: read_string_array walked past the end of the heap from ${ptr}; the array is not NUL-terminated`,
          );
        }
        const strPtr = mod.getValue(slot, '*');
        if (!strPtr) return out;
        out.push(mod.UTF8ToString(strPtr));
      }
    },
  };

  for (const [name, prop] of heaps) {
    out[`${name}_get`] = async (offset, length) => {
      // slice() copies. subarray() would keep a view on the wasm heap, and
      // that view goes stale after _free.
      return m()[prop].slice(offset, offset + length);
    };
    out[`${name}_set`] = async (offset, values) => {
      m()[prop].set(values, offset);
      return ok;
    };
  }

  return out;
};

// The plain ccall method. It sends the four arguments to the module and
// returns what the C function returns. A handler whose library needs no
// marshaling can put this method directly in its methods object.
//
// Some handlers must allocate memory for out-parameters, copy coordinate
// arrays in and out, or walk a returned list of structs. That work is specific
// to the calling conventions of the library, so such a handler writes its own
// ccall in its overrides module.
export const ccallMethod = (getModule) => async (fnName, returnType, argTypes, args) => {
  const mod = getModule();
  if (!mod) throw new Error('handler-heap: getModule() returned null (called before init?)');
  return mod.ccall(fnName, returnType, argTypes, args);
};

export default heapHelpers;
