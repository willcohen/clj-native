// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a decorator module whose body runs in the fetch worker. It
// holds the worker's first exchange of control[0] until the caller posts the
// next request, so the caller takes the first request back before it.
let delayed = false;
const compareExchange = Atomics.compareExchange;
Atomics.compareExchange = (a, i, e, v) => {
  if (!delayed && i === 0 && v === 0) {
    delayed = true;
    Atomics.wait(a, 0, e, 10000);
  }
  return compareExchange(a, i, e, v);
};

export default (request) => request;
