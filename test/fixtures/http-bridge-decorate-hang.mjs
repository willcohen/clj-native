// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a decorator that never resolves. The worker's
// requestTimeoutMs must abort it.
export default function decorate() {
  return new Promise(() => {});
}
