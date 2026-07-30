// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a decorator module that throws at import time. createSyncFetch
// must REJECT rather than fall back to serving requests unauthenticated.
throw new Error('decorator import blew up');
