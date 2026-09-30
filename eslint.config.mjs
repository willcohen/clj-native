// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Lints the hand-written .mjs and the test fixtures. The ignored src files are
// gitignored squint outputs, and dist/ holds the esbuild bundles. test/cljs and test/cljc hold compiled test
// mirrors.
import js from '@eslint/js';
import globals from 'globals';

export default [
  {
    ignores: [
      'src/cljc/net/willcohen/native/dispatch.mjs',
      'src/cljc/net/willcohen/native/macros.mjs',
      'src/cljc/net/willcohen/native/platform_state.mjs',
      'src/cljc/net/willcohen/native/pool.mjs',
      'src/cljc/net/willcohen/native/test_runner.mjs',
      'src/cljc/net/willcohen/native/workload_pool.mjs',
      'dist/**',
      'test/cljs/**',
      'test/cljc/**',
    ],
  },
  js.configs.recommended,
  {
    files: ['**/*.mjs'],
    languageOptions: {
      ecmaVersion: 'latest',
      sourceType: 'module',
      // These modules run in Node, browser pages and workers.
      globals: { ...globals.browser, ...globals.node, ...globals.worker },
    },
  },
];
