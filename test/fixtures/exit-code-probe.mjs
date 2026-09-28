// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a one-test cljs.test namespace under the real test_runner.mjs.
// argv[2] picks the outcome, and test_runner_test.clj checks the exit code.
//
// Written by hand to stay readable. It follows squint output:
// register_test_BANG_ takes a fn with {name, ns} metadata, and `is` expands to
// a `report` call.

import * as squint_core from 'squint-cljs/core.js';
import * as t from 'squint-cljs/src/squint/test.js';
import * as tr from 'ffi-wasm/test-runner';

const NS = 'net.willcohen.native.exit-code-probe';
const mode = process.argv[2];

const MODES = ['pass', 'fail', 'error', 'teardown', 'teardown-fail', 'teardown-ns'];
if (!MODES.includes(mode)) {
  console.error(`exit-code-probe: expected one of ${MODES.join('|')}, got ${mode}`);
  process.exit(2);
}

const shouldPass = mode !== 'fail' && mode !== 'teardown-fail';

// Prints on a later tick, so a runner that does not await it prints nothing.
function teardown() {
  return new Promise((resolve) => {
    setTimeout(() => {
      console.log('TEARDOWN-RAN');
      resolve();
    }, 10);
  });
}

t.register_test_BANG_(
  NS,
  squint_core.with_meta(
    function () {
      if (mode === 'error') {
        throw new Error('exit-code-probe: deliberate throw');
      }
      console.log('TEST-RAN');
      t.report({
        type: shouldPass ? 'pass' : 'fail',
        message: `exit-code-probe ${mode}`,
        expected: true,
        actual: shouldPass,
      });
      return shouldPass;
    },
    { name: `probe-${mode}`, ns: NS },
  ),
);

switch (mode) {
  // A teardown and no names: every registered test runs.
  case 'teardown':
  case 'teardown-fail':
    tr.run_tests_and_exit_BANG_(teardown);
    break;
  // A teardown followed by the name to run.
  case 'teardown-ns':
    tr.run_tests_and_exit_BANG_(teardown, NS);
    break;
  // A name and no teardown.
  default:
    tr.run_tests_and_exit_BANG_(NS);
}
