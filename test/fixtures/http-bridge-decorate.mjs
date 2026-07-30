// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a fetch-worker decorator module. createSyncFetch's Node path
// passes a decorateUrl to the fetch worker, which imports the default export
// and applies it to each request before dispatch. This mirrors the auth seam:
// a real decorator would inject auth headers here (and could await a token
// refresh, since the worker thread is not the blocked caller).
export default async function decorate(request) {
  return {
    ...request,
    headers: { ...request.headers, 'x-injected': 'bridge-decorator' },
  };
}
