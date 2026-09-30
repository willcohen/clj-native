// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Builds dist/: the page bundle and the worker bundle of ffi-wasm's own
// modules, and the files that init-pool! and createSyncFetch load from next to
// them. bb build:js runs it after squint compiles the .cljc modules.
import * as esbuild from 'esbuild';
import { copyFileSync, mkdirSync } from 'node:fs';

const src = 'src/cljc/net/willcohen/native';

const common = {
  bundle: true,
  format: 'esm',
  platform: 'neutral',
  mainFields: ['module', 'main'],
  // Node loads the builtins through dynamic imports on Node only. Each one
  // has the node: prefix, so a consumer that bundles a dist file marks only
  // node:* as external.
  external: ['node:*'],
  logLevel: 'warning',
};

mkdirSync('dist', { recursive: true });

await esbuild.build({
  ...common,
  entryPoints: [`${src}/ffi_wasm.mjs`],
  outfile: 'dist/ffi-wasm.mjs',
  // The bundle holds ffi-wasm's own code only. A copy of another package is a
  // second module instance next to the copy that a consumer imports.
  external: [...common.external, 'squint-cljs', 'resource-tracker', 'worker-router'],
});

await esbuild.build({ ...common, entryPoints: [`${src}/handler.mjs`], outfile: 'dist/handler.mjs' });

copyFileSync(`${src}/fetch_worker.mjs`, 'dist/fetch_worker.mjs');
// Out of the page bundle, so that a page does not import the squint test library.
copyFileSync(`${src}/test_runner.mjs`, 'dist/test_runner.mjs');
