// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a fetch-worker decorator that injects a header, as an auth
// decorator would. It may await, since the worker is not the blocked caller.
export default async function decorate(request) {
  return {
    ...request,
    headers: { ...request.headers, 'x-injected': 'bridge-decorator' },
  };
}
