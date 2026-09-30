// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: the smallest worker-router handler module. ping echoes the
// :args tag, so a test can tell which library's init payload arrived.
// init_arg reads one field of the init args that the worker got.
export function create(initArgs) {
  const tag = (initArgs && initArgs.tag) || '';
  return {
    ping: () => `pong:${tag}`,
    init_arg: (k) => initArgs?.[k] ?? null,
  };
}

export default create;
