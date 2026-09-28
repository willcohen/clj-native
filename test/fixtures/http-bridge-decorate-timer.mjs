// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a decorator whose module timer fires between requests only
// when the fetch worker idles on its event loop.
let timer = 'pending';
setTimeout(() => { timer = 'fired'; }, 0);

export default (request) => ({ ...request, headers: { ...request.headers, 'x-timer': timer } });
