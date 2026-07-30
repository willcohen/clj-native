// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: a one-test cljs.test namespace whose outcome is chosen by
// argv[2] (pass | fail | error | teardown | teardown-fail | teardown-ns), run
// under the real test_runner.mjs so the process exit code is the thing under
// test. test_runner reads squint's report counters by the string keys "fail"
// and "error"; if squint renames either, every CLJS suite in clj-native AND
// clj-proj exits 0 no matter what failed. Driven by test_runner_test.clj,
// which runs on the JVM so its own verdict does not depend on the runner it
// is checking.
//
// The teardown modes cover the other half of the runner's contract: it tells
// a teardown fn apart from a namespace name by type, so consumers reach it
// two different ways. One passes a shutdown fn and no names; another passes
// one name and no teardown. The teardown here resolves on a later tick and
// prints only then, so a runner that failed to await it would exit first and
// print nothing.
//
// Written by hand rather than compiled from .cljc so it stays readable next
// to what squint actually emits: `register_test_BANG_` takes a fn carrying
// {name, ns} metadata, and `is` expands to a `report` call with a "pass" or
// "fail" type.

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
