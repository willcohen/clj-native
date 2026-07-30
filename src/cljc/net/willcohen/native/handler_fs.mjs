// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// MEMFS staging helper for wasm-backed handlers. A worker that runs an
// emscripten module has its own MEMFS. That MEMFS cannot see host paths. To
// let the C library in the worker read a host file, the consumer must first
// copy the bytes across the worker boundary into MEMFS.
//
// stageFiles(module, files, memfsDir) makes the directory, writes the bytes of
// each file, and returns a map of basename to absolute MEMFS path. A repeat
// call on the same directory is safe. The host half of the job stays with the
// consumer, because it is specific to the library and the platform. The
// consumer reads the bytes from disk and groups sibling files that must stage
// together. Call stageFiles from a handler method on the worker side. The host
// reaches it through pool.worker-call.

const EEXIST = 20;

const ensureUint8 = (value, name) => {
  if (value instanceof Uint8Array) return value;
  // A bare ArrayBuffer has a byteLength but no .buffer, so it misses the
  // typed-array arm below. fetch().arrayBuffer() hands one back, and the
  // fingerprint walk in handler-runtime already counts an ArrayBuffer as
  // bytes, so refusing it here would make the package disagree with itself.
  if (value instanceof ArrayBuffer) return new Uint8Array(value);
  if (value && typeof value.byteLength === 'number' && typeof value.buffer !== 'undefined') {
    return new Uint8Array(value.buffer, value.byteOffset ?? 0, value.byteLength);
  }
  if (Array.isArray(value)) return new Uint8Array(value);
  throw new Error('stageFiles: ' + name + ' is not Uint8Array or coercible (got ' + typeof value + ')');
};

const mkdirP = (FS, dir) => {
  // mkdirTree came in with emscripten 1.39. It makes every level of the path,
  // and a repeat call is safe. An older build has only mkdir, which makes one
  // level and throws EEXIST if the directory is already there. Most consumers
  // pass a directory of one level, so the older path is enough for them.
  if (typeof FS.mkdirTree === 'function') {
    FS.mkdirTree(dir);
    return;
  }
  try { FS.mkdir(dir); }
  catch (e) { if (e?.errno !== EEXIST) throw e; }
};

export const stageFiles = (module, files, memfsDir) => {
  if (!module || !module.FS) {
    throw new Error('stageFiles: module.FS not available');
  }
  if (typeof memfsDir !== 'string' || memfsDir.length === 0) {
    throw new Error('stageFiles: memfsDir must be a non-empty string');
  }
  if (!files || typeof files !== 'object') {
    throw new Error('stageFiles: files must be a non-empty object map of name -> bytes');
  }
  mkdirP(module.FS, memfsDir);
  const trimmed = memfsDir.endsWith('/') ? memfsDir.slice(0, -1) : memfsDir;
  const out = {};
  for (const name of Object.keys(files)) {
    const u8 = ensureUint8(files[name], name);
    const path = trimmed + '/' + name;
    module.FS.writeFile(path, u8);
    out[name] = path;
  }
  return out;
};

export default stageFiles;
