// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a decorator that never resolves its first call. The worker's
// requestTimeoutMs must abort it, so the worker can serve the next request.
let calls = 0;

export default function decorate(request) {
  calls += 1;
  return calls === 1 ? new Promise(() => {}) : request;
}
