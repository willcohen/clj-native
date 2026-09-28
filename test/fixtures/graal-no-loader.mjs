// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: not a loader. `initialize` exists but is not a function, so
// bootstrap-graal-module! must check for an executable member.

export const initialize = 'not a function';
