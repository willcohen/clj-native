// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a decorator module that schedules an unhandled rejection after
// the worker reports ready, which ends the fetch worker between requests.
setTimeout(() => Promise.reject(new Error('token refresh failed')), 0);

export default (request) => request;
