// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a module that is not a loader. It exports a plausible name and
// a value under the right name, so the suite pins that bootstrap-graal-module!
// rejects on a missing EXECUTABLE load/initialize rather than on a missing key.

export const setup = () => ({ ok: true });
export const initialize = 'not a function';
