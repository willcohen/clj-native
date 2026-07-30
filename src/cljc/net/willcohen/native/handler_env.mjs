// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Host-environment classification. Node and a browser differ in how a module
// reads a sibling asset, starts a worker, and blocks a synchronous call. Most
// modules here, and in a consumer, first ask which host they run in. The rule
// lives in one place, so it cannot drift between them.
//
// classifyEnvironment(globals) holds the whole rule as a pure function of a
// globals object. A test can therefore give it a stand-in in place of the real
// one. detectEnvironment() applies the rule to globalThis at call time. isNode
// and isBrowser record the same answer once, at import.
//
// isNode is the boolean to branch on. By this rule a Web Worker is neither
// Node nor a browser, because it has no `window`, so it classifies as
// 'unknown'. Code on the worker side that needs the browser path must
// therefore test `!isNode` and not isBrowser.
//
// A module that a GraalVM polyglot Context evaluates cannot import this one.
// Such a module loads as a bare ESM Source with no package resolution, so a
// bare specifier fails. Those modules keep a local copy. The wasm namespace
// records that contract at bootstrap-graal-module!.

// `globals` supplies `process` and `window`. A key that the object does not
// have gives the same answer as a global that does not exist.
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
