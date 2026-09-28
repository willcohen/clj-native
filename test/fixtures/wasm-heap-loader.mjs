// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: an emscripten-shaped Module over a real WebAssembly.Memory,
// behind the `initialize(opts)` -> onSuccess(module) contract. The wasm bytes
// are written out, so the suite needs no emcc or wabt. Only the memory and the
// allocator are wasm.

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

// GraalJS has no TextEncoder/TextDecoder. escape and unescape map a string
// to and from a binary string of its UTF-8 bytes.
function utf8Decode(heap, ptr) {
  let bin = '';
  for (let i = ptr; heap[i]; i++) bin += String.fromCharCode(heap[i]);
  return decodeURIComponent(escape(bin));
}

function utf8Encode(str, heap, ptr, maxBytes) {
  if (maxBytes <= 0) return 0;
  const bin = unescape(encodeURIComponent(str));
  const n = Math.min(bin.length, maxBytes - 1);
  for (let i = 0; i < n; i++) heap[ptr + i] = bin.charCodeAt(i);
  heap[ptr + n] = 0;
  return n;
}

function makeModule() {
  const instance = new WebAssembly.Instance(new WebAssembly.Module(MODULE_BYTES));
  const { memory, malloc, free } = instance.exports;
  const buffer = memory.buffer;

  // Plain properties: this memory never grows, so the views need no refresh.
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

  // type -> [view, shift]. "*" is "i32" on wasm32, a 4-byte pointer slot.
  const views = {
    '*': [M.HEAP32, 2],
    i32: [M.HEAP32, 2],
    i8: [M.HEAP8, 0],
    i16: [M.HEAP16, 1],
    float: [M.HEAPF32, 2],
    double: [M.HEAPF64, 3],
  };
  const viewOf = (type) => {
    const v = views[type];
    if (!v) throw new Error(`wasm-heap-loader: unsupported type ${type}`);
    return v;
  };
  M.getValue = (ptr, type) => {
    const [heap, shift] = viewOf(type);
    return heap[ptr >> shift];
  };
  M.setValue = (ptr, value, type) => {
    const [heap, shift] = viewOf(type);
    heap[ptr >> shift] = value;
  };

  M.UTF8ToString = (ptr) => (ptr ? utf8Decode(M.HEAPU8, ptr) : '');
  M.stringToUTF8 = (str, ptr, maxBytes) => utf8Encode(str, M.HEAPU8, ptr, maxBytes);

  // emscripten's ccall reads its two lists as JS arrays, and a host array
  // sends nothing. This stand-in records such a list as null and answers -1.
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
      // As in emscripten, rettype "string" gives "" for NULL and for an empty
      // string. Rettype "number" gives the address.
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
