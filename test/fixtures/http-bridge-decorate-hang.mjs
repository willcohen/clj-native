// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a decorator that never resolves. The worker's request timeout
// must abort it so the blocked Atomics.wait caller unblocks with a status-0
// transport failure instead of deadlocking forever.
export default function decorate() {
  return new Promise(() => {});
}
