// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: the overrides module of a generated handler. probe reports
// what the handler gave the overrides, so gen_handler_test can check it.
let initFfi = null;
let initArgs = null;

export async function init(args, ctx) {
  initFfi = ctx.ffi;
  initArgs = args;
}

export const methods = (ffi) => ({
  probe: async () => ({
    sameFfi: ffi === initFfi,
    makeHandler: typeof ffi.makeHandler,
    stageFiles: typeof ffi.stageFiles,
    createSyncFetch: typeof ffi.createSyncFetch,
    tag: initArgs?.tag ?? null,
  }),
});

export function destroy() {
  return 'destroyed';
}
