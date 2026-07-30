// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a module that imports cleanly but exports no decorator
// function. A decorateUrl that resolves to a non-function is as much an auth
// drop as an import failure, so createSyncFetch must REJECT here too.
export const notADecorator = 42;
