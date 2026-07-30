// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture for the `load` loader contract bootstrap-graal-module! prefers.
// It reports what the options carried, so the suite checks the byte encoding as
// well as the promise-to-future bridge.
//
// `load` is deliberately not declared async: an async function always returns a
// promise, and one of the cases under test is a loader that returns the module
// itself. options.mode selects the case:
//   'ok'     resolve with a module-shaped object, after a real await
//   'throw'  reject, so the error path runs
//   'sync'   return the module directly, with no promise at all

function describe(options) {
  const db = options.dbBytes;
  const grids = options.grids;
  return {
    marker: 'load-contract',
    mode: options.mode,
    dbIsUint8Array: db instanceof Uint8Array,
    dbLength: db ? db.length : -1,
    // A Java byte array arrives with signed bytes; these two pin the widening.
    dbSecondLast: db ? db[db.length - 2] : -1,
    dbLast: db ? db[db.length - 1] : -1,
    dbBufferBytes: db ? db.buffer.byteLength : -1,
    gridNames: grids ? Object.keys(grids).sort().join(',') : '',
    gridFirstByte: grids ? grids['b.gsb'][0] : -1,
  };
}

export function load(options) {
  if (options.mode === 'sync') return describe(options);
  return (async () => {
    await Promise.resolve();
    if (options.mode === 'throw') {
      throw new Error('graal-load-loader: refusing on purpose');
    }
    return describe(options);
  })();
}
