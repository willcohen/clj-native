// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Host classification in one place, to keep modules from drifting apart.
// classifyEnvironment is a pure function of a globals object, and a test can
// pass a stand-in. detectEnvironment() applies it to globalThis at call time.
// isNode and isBrowser hold the answer from import time.
//
// A Web Worker has no `window` and classifies as 'unknown'. Worker code that
// needs the browser path tests `!isNode`, not isBrowser.
//
// A module that a GraalVM polyglot Context evaluates cannot import this one.
// It loads as a bare ESM Source with no package resolution, and keeps a local
// copy.

// Reads only `process` and `window`. A missing key counts as a missing global.
export const classifyEnvironment = (globals) => {
  const g = globals ?? {};
  const proc = g.process;
  if (proc && proc.versions != null && proc.versions.node != null) return 'node';
  const win = g.window;
  if (win && typeof win.document !== 'undefined') return 'browser';
  return 'unknown';
};

export const detectEnvironment = () => classifyEnvironment(globalThis);

export const isNode = detectEnvironment() === 'node';
export const isBrowser = detectEnvironment() === 'browser';

export default { classifyEnvironment, detectEnvironment, isNode, isBrowser };
