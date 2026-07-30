// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: the smallest worker-router handler module. The
// workload-pool registry suite registers this under two lib-keys with
// different :args payloads; ping echoes the payload's tag back, which
// proves the registry's fold delivered each library's own init payload
// into the one joint pool.
export function create(initArgs) {
  const tag = (initArgs && initArgs.tag) || '';
  return {
    ping: () => `pong:${tag}`,
  };
}

export default create;
