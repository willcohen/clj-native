// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: stands in for emscripten MODULARIZE output, a default-export
// factory that resolves a module.

export default () => Promise.resolve({
  marker: 'fake-emscripten',
});
