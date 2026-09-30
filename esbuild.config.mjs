// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Builds dist/: the page bundle of ffi-wasm's own modules, and next to it the
// worker entry, the modules that the page and the worker share, and the files
// that createSyncFetch and a test load. bb build:js runs it after squint
// compiles the .cljc modules.
import * as esbuild from 'esbuild';
import { copyFileSync, mkdirSync } from 'node:fs';

const src = 'src/cljc/net/willcohen/native';

// Each is a file of its own in dist/, and each importer imports it by a
// relative path, so a realm that loads the page bundle and handler.mjs loads
// one copy of each. They import only each other and node: builtins, thus a
// module worker needs no importmap for them.
const shared = ['handler_runtime', 'handler_env', 'handler_fs', 'handler_heap',
                'handler_paths', 'http_bridge'].map((m) => `${m}.mjs`);

const keepSharedExternal = {
  name: 'keep-shared-external',
  setup(build) {
    build.onResolve({ filter: /^\.\/[a-z_]+\.mjs$/ }, (args) =>
      shared.includes(args.path.slice(2)) ? { path: args.path, external: true } : undefined);
  },
};

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
  plugins: [keepSharedExternal],
});

// test_runner.mjs is out of the page bundle, so that a page does not import
// the squint test library.
for (const f of ['handler.mjs', ...shared, 'fetch_worker.mjs', 'test_runner.mjs']) {
  copyFileSync(`${src}/${f}`, `dist/${f}`);
}
