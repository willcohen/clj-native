// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// The page side of ffi-wasm as one module. esbuild bundles it into
// dist/ffi-wasm.mjs, and each subpath export except ./handler, ./fetch-worker
// and ./test-runner points there. A page, Node and a bundler then load one module
// instance. export * drops a name that two of these modules export, and
// bundle_test checks that none does.

export * from './pool.mjs';
export * from './dispatch.mjs';
export * from './platform_state.mjs';
export * from './macros.mjs';
export * from './workload_pool.mjs';
export * from './handler_runtime.mjs';
export * from './handler_env.mjs';
export * from './handler_fs.mjs';
export * from './handler_heap.mjs';
export * from './handler_paths.mjs';
export * from './http_bridge.mjs';
