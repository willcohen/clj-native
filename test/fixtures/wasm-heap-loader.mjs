// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: the smallest thing graal_wasm.clj can talk to. Exposes the
// `initialize(opts)` -> onSuccess(module) contract that
// bootstrap-graal-module! drives, and hands back an emscripten-shaped Module
// backed by a real WebAssembly.Memory, so the JVM code under test reaches
// genuine wasm memory through polyglot rather than a JS stand-in.
//
// The module bytes are written out below instead of compiled, so this suite
// needs neither emcc nor wabt. Everything emscripten normally generates in JS
// (the HEAP views, getValue/setValue, the UTF-8 pair) is JS here too; only the
// memory and the allocator are wasm, which is exactly the boundary wasm.cljc
// crosses.

// (module
//   (memory 2)
//   (global $bump (mut i32) (i32.const 1024))
//   (func $malloc (param $size i32) (result i32) (local $ptr i32)
//     global.get $bump  local.set $ptr
//     global.get $bump  local.get $size  i32.add
//     i32.const 7  i32.add  i32.const -8  i32.and
//     global.set $bump
//     local.get $ptr)
//   (func $free (param $ptr i32))          ;; bump allocator: never reclaims
//   (export "memory" (memory 0))
//   (export "malloc" (func $malloc))
//   (export "free" (func $free)))
const MODULE_BYTES = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, // magic, version 1

  // type section: 0 = (i32)->i32, 1 = (i32)->()
  0x01, 0x0a, 0x02,
  0x60, 0x01, 0x7f, 0x01, 0x7f,
  0x60, 0x01, 0x7f, 0x00,

  // function section: func0 uses type0, func1 uses type1
  0x03, 0x03, 0x02, 0x00, 0x01,

  // memory section: one memory, minimum 2 pages (128 KiB)
  0x05, 0x03, 0x01, 0x00, 0x02,

  // global section: one mutable i32, initialized to 1024 so address 0 is NULL
  0x06, 0x07, 0x01, 0x7f, 0x01, 0x41, 0x80, 0x08, 0x0b,

  // export section: "memory", "malloc", "free"
  0x07, 0x1a, 0x03,
  0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x02, 0x00,
  0x06, 0x6d, 0x61, 0x6c, 0x6c, 0x6f, 0x63, 0x00, 0x00,
  0x04, 0x66, 0x72, 0x65, 0x65, 0x00, 0x01,

  // code section
  0x0a, 0x1c, 0x02,
  0x17, 0x01, 0x01, 0x7f, // $malloc, one i32 local
  0x23, 0x00, 0x21, 0x01, // $ptr = $bump
  0x23, 0x00, 0x20, 0x00, 0x6a, 0x41, 0x07, 0x6a, 0x41, 0x78, 0x71, 0x24, 0x00,
  0x20, 0x01, 0x0b, //        $bump = ($bump + $size + 7) & -8 ; return $ptr
  0x02, 0x00, 0x0b, //        $free, empty body
]);

// Hand-rolled UTF-8, in the shape emscripten emits: GraalVM's JS has no
// TextEncoder/TextDecoder (they are WHATWG, not ECMAScript), so the pair the
// string round-trip depends on has to be written out.
function utf8Decode(heap, ptr) {
  let str = '';
  let i = ptr;
  while (heap[i]) {
    const u0 = heap[i++];
    if (!(u0 & 0x80)) {
      str += String.fromCharCode(u0);
      continue;
    }
    const u1 = heap[i++] & 63;
    if ((u0 & 0xe0) === 0xc0) {
      str += String.fromCharCode(((u0 & 31) << 6) | u1);
      continue;
    }
    const u2 = heap[i++] & 63;
    const u = (u0 & 0xf0) === 0xe0
      ? ((u0 & 15) << 12) | (u1 << 6) | u2
      : ((u0 & 7) << 18) | (u1 << 12) | (u2 << 6) | (heap[i++] & 63);
    if (u <= 0xffff) {
      str += String.fromCharCode(u);
    } else {
      const ch = u - 0x10000;
      str += String.fromCharCode(0xd800 | (ch >> 10), 0xdc00 | (ch & 0x3ff));
    }
  }
  return str;
}

function utf8Encode(str, heap, outIdx, maxBytesToWrite) {
  if (maxBytesToWrite <= 0) return 0;
  const startIdx = outIdx;
  const endIdx = outIdx + maxBytesToWrite - 1; // the NUL slot
  for (let i = 0; i < str.length; ++i) {
    let u = str.charCodeAt(i);
    if (u >= 0xd800 && u <= 0xdfff) {
      u = (0x10000 + ((u & 0x3ff) << 10)) | (str.charCodeAt(++i) & 0x3ff);
    }
    if (u <= 0x7f) {
      if (outIdx >= endIdx) break;
      heap[outIdx++] = u;
    } else if (u <= 0x7ff) {
      if (outIdx + 1 >= endIdx) break;
      heap[outIdx++] = 0xc0 | (u >> 6);
      heap[outIdx++] = 0x80 | (u & 63);
    } else if (u <= 0xffff) {
      if (outIdx + 2 >= endIdx) break;
      heap[outIdx++] = 0xe0 | (u >> 12);
      heap[outIdx++] = 0x80 | ((u >> 6) & 63);
      heap[outIdx++] = 0x80 | (u & 63);
    } else {
      if (outIdx + 3 >= endIdx) break;
      heap[outIdx++] = 0xf0 | (u >> 18);
      heap[outIdx++] = 0x80 | ((u >> 12) & 63);
      heap[outIdx++] = 0x80 | ((u >> 6) & 63);
      heap[outIdx++] = 0x80 | (u & 63);
    }
  }
  heap[outIdx] = 0;
  return outIdx - startIdx;
}

function makeModule() {
  const instance = new WebAssembly.Instance(new WebAssembly.Module(MODULE_BYTES));
  const { memory, malloc, free } = instance.exports;
  const buffer = memory.buffer;

  // Plain properties, not getters: this memory has no grow path, so the views
  // never need the refresh emscripten does after a growth.
  const M = {
    HEAP8: new Int8Array(buffer),
    HEAPU8: new Uint8Array(buffer),
    HEAP16: new Int16Array(buffer),
    HEAPU16: new Uint16Array(buffer),
    HEAP32: new Int32Array(buffer),
    HEAPU32: new Uint32Array(buffer),
    HEAPF32: new Float32Array(buffer),
    HEAPF64: new Float64Array(buffer),
  };

  M._malloc = (size) => malloc(size);
  M._free = (ptr) => free(ptr);

  // emscripten's type tags. "*" is "i32" on wasm32, which is what makes the
  // 4-byte pointer slots in pointers->wasm-array and string-list-to-native-array
  // line up.
  M.getValue = (ptr, type) => {
    switch (type) {
      case '*':
      case 'i32': return M.HEAP32[ptr >> 2];
      case 'i8': return M.HEAP8[ptr];
      case 'i16': return M.HEAP16[ptr >> 1];
      case 'float': return M.HEAPF32[ptr >> 2];
      case 'double': return M.HEAPF64[ptr >> 3];
      default: throw new Error(`wasm-heap-loader getValue: unsupported type ${type}`);
    }
  };

  M.setValue = (ptr, value, type) => {
    switch (type) {
      case '*':
      case 'i32': M.HEAP32[ptr >> 2] = value; break;
      case 'i8': M.HEAP8[ptr] = value; break;
      case 'i16': M.HEAP16[ptr >> 1] = value; break;
      case 'float': M.HEAPF32[ptr >> 2] = value; break;
      case 'double': M.HEAPF64[ptr >> 3] = value; break;
      default: throw new Error(`wasm-heap-loader setValue: unsupported type ${type}`);
    }
  };

  M.UTF8ToString = (ptr) => (ptr ? utf8Decode(M.HEAPU8, ptr) : '');
  M.stringToUTF8 = (str, ptr, maxBytes) => utf8Encode(str, M.HEAPU8, ptr, maxBytes);

  // A stand-in for emscripten's ccall, which reads its two list arguments as
  // JS arrays. A host array arrives here with no length, no index access and
  // no array methods, so a caller that passes one sends nothing at all -- and
  // the call still returns a value. This stand-in reports that state instead
  // of hiding it: it records null lists and answers -1.
  M.__ccalls = [];
  M.ccall = (name, rettype, argtypes, args) => {
    const types = Array.isArray(argtypes) ? Array.from(argtypes) : null;
    const vals = Array.isArray(args) ? Array.from(args) : null;
    M.__ccalls.push({ name, rettype, types, vals });
    if (types === null || vals === null) return -1;
    switch (name) {
      // Adds its arguments, so a test sees whether they arrived.
      case 'sum':
        return vals.reduce((a, b) => a + b, 0);
      // Reads a host callback off globalThis and calls it, the way a
      // library's own C stub reaches a registered callback.
      case 'call_global': {
        const fn = globalThis[vals[0]];
        return typeof fn === 'function' ? fn(vals[1]) : -1;
      }
      // A char* result. As emscripten ccall does, rettype "string" gives
      // UTF8ToString of the address, which is "" for NULL and for an empty
      // string alike. Rettype "number" gives the address itself.
      case 'str_null':
      case 'str_empty':
      case 'str_abc': {
        let ptr = 0;
        if (name !== 'str_null') {
          const text = name === 'str_abc' ? 'abc' : '';
          ptr = M._malloc(text.length + 1);
          M.stringToUTF8(text, ptr, text.length + 1);
        }
        return rettype === 'string' ? M.UTF8ToString(ptr) : ptr;
      }
      // An i64 result, as a WASM_BIGINT module gives it: a BigInt.
      case 'i64_ret':
        return 3000000000n;
      // An i64 parameter of a WASM_BIGINT module rejects a Number, as the
      // wasm boundary does ("Cannot convert 1900 to a BigInt").
      case 'i64_echo':
        if (typeof vals[0] !== 'bigint') {
          throw new TypeError(`Cannot convert ${vals[0]} to a BigInt`);
        }
        return vals[0] + 1n;
      default:
        return 0;
    }
  };

  return M;
}

// bootstrap-graal-module! injects onSuccess / onError into the opts it passes.
export function initialize(opts) {
  try {
    opts.onSuccess(makeModule());
  } catch (e) {
    opts.onError(String(e));
  }
}
