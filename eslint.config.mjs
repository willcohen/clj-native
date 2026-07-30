// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Lints the hand-written .mjs only. The six ignored src files are squint
// build outputs (gitignored); test/cljs and test/cljc .mjs are compiled
// test mirrors. eslint therefore covers: the seven hand-written runtime
// modules under src/cljc/net/willcohen/native/ and every test fixture.
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
      // These modules run across Node, browser pages, and workers; the
      // union keeps host-specific globals (process, self, WebAssembly,
      // SharedArrayBuffer) resolvable in every file.
      globals: { ...globals.browser, ...globals.node, ...globals.worker },
    },
    rules: {
      // A catch binding named with a leading underscore is deliberately
      // unused; an empty catch is the deliberate swallow idiom (each site
      // carries a comment saying why).
      'no-unused-vars': ['error', { caughtErrorsIgnorePattern: '^_' }],
      'no-empty': ['error', { allowEmptyCatch: true }],
    },
  },
];
