// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Asset-path resolution for wasm-backed handlers. A handler must find sibling
// files, such as a database or a secondary .mjs or .wasm. Those files sit at
// different depths in different layouts. A consumer that runs from source
// finds them some levels up, under resources/. A consumer that runs from a
// bundled distribution finds them in the same directory. The probe is the same
// in every consumer, and it is easy to get the number of levels wrong.
//
// resolveAsset(importMetaUrl, name, candidates) returns the first candidate
// directory that holds `name`. If no candidate holds it, the function throws
// and lists every path it probed. Node only.
//
// loadEmscriptenModule uses resolveAsset. It imports the emscripten output of
// a consumer and returns the module factory with a matching locateFile. On
// Node it resolves through resolveAsset. In a browser it resolves against
// import.meta.url.

import { isNode } from './handler_env.mjs';

const ensureNode = () => {
  if (!isNode) {
    throw new Error('handler-paths: node-only (browser path not yet wired)');
  }
};

// Make a candidate into an absolute directory. An absolute string passes
// through unchanged. An array of segments resolves against the __dirname of
// the caller, for example ['..','..','resources'], which is the usual form. A
// relative string also resolves against that __dirname.
const resolveCandidate = async (here, candidate) => {
  const { resolve, isAbsolute } = await import('node:path');
  if (Array.isArray(candidate)) return resolve(here, ...candidate);
  if (typeof candidate !== 'string') {
    throw new Error('handler-paths: candidate must be a string or array of segments (got ' + typeof candidate + ')');
  }
  if (isAbsolute(candidate)) return candidate;
  return resolve(here, candidate);
};

// importMetaUrl is the import.meta.url of the caller. A relative candidate
// resolves from the file location of the caller, not from clj-native.
export const resolveAsset = async (importMetaUrl, name, candidates) => {
  ensureNode();
  if (typeof name !== 'string' || name.length === 0) {
    throw new Error('handler-paths: name must be a non-empty string');
  }
  if (!Array.isArray(candidates) || candidates.length === 0) {
    throw new Error('handler-paths: candidates must be a non-empty array');
  }
  const { fileURLToPath } = await import('node:url');
  const { dirname, join } = await import('node:path');
  const { existsSync } = await import('node:fs');
  const here = dirname(fileURLToPath(importMetaUrl));
  const tried = [];
  for (const c of candidates) {
    const dir = await resolveCandidate(here, c);
    tried.push(dir);
    if (existsSync(join(dir, name))) {
      return { dir, path: join(dir, name) };
    }
  }
  throw new Error('handler-paths: ' + name + ' not found on any candidate: ' + tried.join(', '));
};

// Import the emscripten output of a consumer and return the parts needed to
// start it. On Node the result is {factory, locateFile, dir}. In a browser it
// is {factory, locateFile, baseUrl}. No branch returns both dir and baseUrl.
//
// `factory` is the default export of the module, which is the function that
// emscripten MODULARIZE emits. The caller invokes it with its own module
// arguments. `locateFile` maps the name of a sibling file, such as the .wasm
// or a .data pack, to the place this import found it. Pass it straight to the
// factory. A caller whose output finds its own siblings can ignore it.
//
// On Node the resolution goes through resolveAsset, so one candidate list
// finds both a database and the module. In a browser the resolution is against
// importMetaUrl. That keeps a bundled distribution correct from any directory,
// including the ./dist/ layout that a CDN uses, with no server-relative path.
//
// opts:
//   name         asset name. Both runtimes use it if no override is given.
//   nodeName     override for Node only, for example a single-threaded build
//   browserName  override for a browser only, for example a pthreads build
//   candidates   candidate directories for the Node probe. See resolveAsset.
export const loadEmscriptenModule = async (importMetaUrl, opts) => {
  const { name, nodeName = name, browserName = name, candidates } = opts ?? {};
  if (typeof nodeName !== 'string' || typeof browserName !== 'string') {
    throw new Error('handler-paths: loadEmscriptenModule needs a `name` (or both `nodeName` and `browserName`)');
  }
  if (isNode) {
    const { dir } = await resolveAsset(importMetaUrl, nodeName, candidates);
    const { pathToFileURL } = await import('node:url');
    const { join } = await import('node:path');
    const imported = await import(pathToFileURL(join(dir, nodeName)).href);
    return { factory: imported.default, locateFile: (n) => join(dir, n), dir };
  }
  const baseUrl = new URL('./', importMetaUrl).href;
  const imported = await import(baseUrl + browserName);
  return { factory: imported.default, locateFile: (n) => baseUrl + n, baseUrl };
};

export default { resolveAsset, loadEmscriptenModule };
