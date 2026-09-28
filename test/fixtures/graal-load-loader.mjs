// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture for the `load` loader contract. The module reports what the
// options carried. `load` is not async, so 'sync' mode can return the module
// itself. 'ok' resolves after an await, and 'throw' rejects.

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
