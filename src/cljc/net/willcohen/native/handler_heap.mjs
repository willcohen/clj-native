// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Emscripten heap views, getValue, setValue, the UTF-8 helpers and a
// string-array walk as one methods object. A handler spreads it into its
// `methods`. getModule binds late, and the object can exist before init()
// loads the module.
//
// An offset is a typed-array index: pass ptr / 4 for heap32 and ptr / 8 for
// heapf64. A heap*_set past the end throws RangeError.
// A heap*_get past the end returns a short array.
//
// No heap64 pair yet. HEAP64 needs WASM_BIGINT, the emscripten default since
// 4.0.0. Add the pair when a consumer needs it. heap*_get returns a typed
// array, which structured clone copies as one block. Every
// method is async because handler-runtime requires it.

// Shared result for methods with nothing to return. The freeze makes a write
// to it throw instead of changing every later result.
const ok = Object.freeze({ ok: true });

// emcc-link builds wasm32. A MEMORY64 build would widen the pointer slot.
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

    // The JVM twin is graal-wasm/string-array-pointer->strs. The heap bound
    // makes an unterminated array throw: a read past the end gives undefined,
    // which would read as the terminator. Nothing here allocates, so one length
    // read holds.
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
      // slice() copies. A subarray() view goes stale after _free.
      return m()[prop].slice(offset, offset + length);
    };
    out[`${name}_set`] = async (offset, values) => {
      m()[prop].set(values, offset);
      return ok;
    };
  }

  return out;
};

// A plain ccall method for a library that needs no marshaling. A handler that
// must allocate out-parameters or copy arrays writes its own ccall in its
// overrides module.
export const ccallMethod = (getModule) => async (fnName, returnType, argTypes, args) => {
  const mod = getModule();
  if (!mod) throw new Error('handler-heap: getModule() returned null (called before init?)');
  return mod.ccall(fnName, returnType, argTypes, args);
};

export default heapHelpers;
