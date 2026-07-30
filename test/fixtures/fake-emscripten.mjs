// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture standing in for a consumer's emscripten MODULARIZE output: one
// default export, a factory taking module arguments and resolving a module.
// loadEmscriptenModule only has to find it, import it and hand back the default
// export, so nothing here needs to be real wasm.

export default () => Promise.resolve({
  marker: 'fake-emscripten',
});
