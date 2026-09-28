// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// HTTP transport for a wasm library whose C runtime issues blocking XHR, such
// as emscripten FETCH.
//
// createSyncFetch(opts) resolves to syncFetch(url, reqOpts), which returns
// {status, headers, bodyBytes}. On Node, syncFetch blocks on Atomics while a
// fetch_worker does the request. In a browser it is a synchronous XHR, and the
// caller must be in a Web Worker.
//
// Contracts:
//   - On Node, createSyncFetch rejects if the worker cannot become ready, for
//     example when the decorator import fails. A later call reuses the
//     running worker and ignores its own workerUrl, dataBufferSize and
//     requestTimeoutMs.
//     It rejects when its decorateUrl differs, since one worker applies one
//     decorator.
//   - A response larger than dataBufferSize gives {status: 0, overflow: true}.
//   - A worker that stops answering gives status 0 after requestTimeoutMs plus
//     slack. A worker that ended gives status 0 at once, after this thread
//     handles its exit event.
//   - A response that the caller gave up on never answers the next request.
//
// The auth decorator runs in the fetch worker, never in the blocked caller.
// installXhrPolyfill sends the synchronous path of a global XHR through
// syncFetch.

import { isNode } from './handler_env.mjs';

const CONTROL_BUFFER_SIZE = 8;
const META_BUFFER_SIZE = 16;
const DEFAULT_DATA_BUFFER_SIZE = 50 * 1024 * 1024;
const DEFAULT_REQUEST_TIMEOUT_MS = 35000;
// The caller waits past the worker's idle timeout, so a stall comes back as
// the worker's status 0.
const WAIT_SLACK_MS = 5000;
const WORKER_READY_TIMEOUT_MS = 10000;
// metaBuffer[3] bit 0: the response did not fit dataBuffer.
const OVERFLOW_FLAG = 1;
// Skips 0, which marks no pending request.
function nextGeneration(state) {
  const next = (state.generation + 1) | 0;
  state.generation = next === 0 ? 1 : next;
  return state.generation;
}

// One fetch worker per thread, shared and reference-counted, because one pool
// worker can load the handlers of several libraries. Each createSyncFetch on
// Node takes a reference, and shutdown() releases one.
let workerState = null;
let workerStart = null;
let refCount = 0;

function parseHeaders(str) {
  const headers = {};
  if (!str) return headers;
  for (const line of str.split(/\r\n|\n/)) {
    const idx = line.indexOf(':');
    if (idx > 0) {
      headers[line.slice(0, idx).trim().toLowerCase()] = line.slice(idx + 1).trim();
    }
  }
  return headers;
}

async function ensureWorker(opts) {
  const decorateUrl = opts.decorateUrl ? String(opts.decorateUrl) : null;
  let state = workerState;
  if (!state) {
    workerStart ??= startWorker(opts.workerUrl, decorateUrl, opts)
      .finally(() => { workerStart = null; });
    state = await workerStart;
  }
  if (state.decorateUrl !== decorateUrl) {
    throw new Error(`createSyncFetch: the fetch worker of this thread has decorateUrl ${state.decorateUrl}, not ${decorateUrl}`);
  }
  return state;
}

async function startWorker(workerUrl, decorateUrl, opts) {
  if (!workerUrl) throw new Error('createSyncFetch: workerUrl is required on Node');
  const { Worker } = await import('worker_threads');

  const dataBufferSize = opts.dataBufferSize ?? DEFAULT_DATA_BUFFER_SIZE;
  const requestTimeoutMs = opts.requestTimeoutMs ?? DEFAULT_REQUEST_TIMEOUT_MS;

  const controlSAB = new SharedArrayBuffer(CONTROL_BUFFER_SIZE);
  const metaSAB = new SharedArrayBuffer(META_BUFFER_SIZE);
  const dataSAB = new SharedArrayBuffer(dataBufferSize);

  const worker = new Worker(workerUrl);
  worker.unref();
  await waitForWorkerReady(worker, {
    cmd: 'init',
    controlBuffer: controlSAB,
    metaBuffer: metaSAB,
    dataBuffer: dataSAB,
    decorateUrl,
    requestTimeoutMs,
  });

  const state = {
    worker,
    decorateUrl,
    control: new Int32Array(controlSAB),
    meta: new Int32Array(metaSAB),
    data: new Uint8Array(dataSAB),
    view: new DataView(dataSAB),
    waitTimeoutMs: requestTimeoutMs + WAIT_SLACK_MS,
    generation: 0,
    dead: false,
  };
  // Without an error listener, an error in the worker, such as a decorator
  // rejection, throws in this thread.
  worker.on('error', (err) => console.error(`http-bridge: fetch worker error: ${err.message}`));
  // refCount stays, since each consumer of the dead worker still releases once.
  worker.on('exit', () => {
    state.dead = true;
    if (workerState === state) workerState = null;
  });
  workerState = state;
  return state;
}

// A rejection terminates the worker, so a later createSyncFetch starts clean.
function waitForWorkerReady(worker, initMsg) {
  return new Promise((resolve, reject) => {
    let settled = false;
    const timer = setTimeout(
      () => finishReject(new Error(
        `createSyncFetch: fetch worker did not become ready within ${WORKER_READY_TIMEOUT_MS}ms`)),
      WORKER_READY_TIMEOUT_MS);
    timer.unref?.();
    const onMessage = (msg) => {
      if (msg?.status === 'ready') finishResolve();
      else if (msg?.status === 'error') {
        finishReject(new Error(`createSyncFetch: fetch worker failed to initialize: ${msg.error}`));
      }
    };
    const onError = (err) =>
      finishReject(new Error(`createSyncFetch: fetch worker error during init: ${err.message}`));
    const onExit = (code) =>
      finishReject(new Error(`createSyncFetch: fetch worker exited (code ${code}) during init`));
    function cleanup() {
      clearTimeout(timer);
      worker.off('message', onMessage);
      worker.off('error', onError);
      worker.off('exit', onExit);
    }
    function finishResolve() {
      if (settled) return;
      settled = true;
      cleanup();
      resolve();
    }
    function finishReject(err) {
      if (settled) return;
      settled = true;
      cleanup();
      worker.terminate();
      reject(err);
    }
    worker.on('message', onMessage);
    worker.on('error', onError);
    worker.on('exit', onExit);
    worker.postMessage(initMsg);
  });
}

function toRequest(url, reqOpts) {
  return {
    url,
    method: reqOpts.method || 'GET',
    headers: reqOpts.headers || {},
    body: reqOpts.body ?? null,
  };
}

const failedResponse = () => ({ status: 0, headers: {}, bodyBytes: new Uint8Array(0) });

// True if the worker answers `generation` before the deadline.
function responseArrived(control, generation, deadline) {
  for (;;) {
    const answered = Atomics.load(control, 1);
    if (answered === generation) return true;
    const remaining = deadline - Date.now();
    if (remaining <= 0) return false;
    Atomics.wait(control, 1, answered, remaining);
  }
}

function nodeSyncFetch(state, url, reqOpts) {
  if (state.dead) return failedResponse();
  const { control, meta, data, view, waitTimeoutMs } = state;
  const request = toRequest(url, reqOpts);
  if (request.body != null && typeof request.body !== 'string') {
    throw new TypeError('http-bridge: on Node, a request body must be a string');
  }
  const jsonBytes = new TextEncoder().encode(JSON.stringify(request));
  view.setInt32(0, jsonBytes.length, true);
  data.set(jsonBytes, 4);

  const generation = nextGeneration(state);
  Atomics.store(control, 0, generation);
  Atomics.notify(control, 0, 1);

  // At the deadline, take the request back: a compareExchange of control[0]
  // from generation to 0. The worker makes the same exchange before it writes,
  // so when the worker took it first, its response is on the way.
  const answered = responseArrived(control, generation, Date.now() + waitTimeoutMs)
    || (Atomics.compareExchange(control, 0, generation, 0) !== generation
        && responseArrived(control, generation, Date.now() + WAIT_SLACK_MS));
  if (!answered) {
    console.error(
      `http-bridge: fetch worker did not answer within ${waitTimeoutMs}ms for ${url}. ` +
      'Returning status 0.');
    return failedResponse();
  }

  const flags = Atomics.load(meta, 3);
  const status = Atomics.load(meta, 0);
  if (flags & OVERFLOW_FLAG) {
    // `overflow` tells this apart from a network error.
    console.error(
      `http-bridge: the response for ${url} is larger than the ${data.length}-byte transport ` +
      `buffer. Upstream status ${status}. Returning status 0.`);
    return { ...failedResponse(), overflow: true };
  }

  const bodyLength = Atomics.load(meta, 1);
  const headersLength = Atomics.load(meta, 2);
  const bodyBytes = data.slice(0, bodyLength);
  const headersStr = new TextDecoder().decode(data.slice(bodyLength, bodyLength + headersLength));
  return { status, headers: parseHeaders(headersStr), bodyBytes };
}

// A synchronous XHR takes a responseType only in a Web Worker.
function browserSyncFetch(url, reqOpts) {
  const request = toRequest(url, reqOpts);
  const xhr = new XMLHttpRequest();
  xhr.open(request.method, request.url, false);
  xhr.responseType = 'arraybuffer';
  for (const [k, v] of Object.entries(request.headers)) xhr.setRequestHeader(k, v);
  xhr.send(request.body);
  return {
    status: xhr.status,
    headers: parseHeaders(xhr.getAllResponseHeaders()),
    bodyBytes: new Uint8Array(xhr.response ?? 0),
  };
}

export async function createSyncFetch(opts = {}) {
  if (isNode) {
    const state = await ensureWorker(opts);
    refCount++;
    return (url, reqOpts = {}) => nodeSyncFetch(state, url, reqOpts);
  }
  return (url, reqOpts = {}) => browserSyncFetch(url, reqOpts);
}

function makeXhrClass(syncFetch, XHR2) {
  return class XMLHttpRequest {
    constructor() {
      this._xhr2 = null;  // Only the async path needs xhr2.
      this._async = true;
      this._method = 'GET';
      this._url = null;
      this._headers = {};
      this._syncResponseHeaders = null;
      this._status = 0;

      ['readyState', 'status', 'statusText', 'response', 'responseText', 'responseType',
        'responseURL', 'onreadystatechange', 'onload', 'onerror', 'onprogress'].forEach((prop) => {
        Object.defineProperty(this, prop, {
          get: () => (this._async && this._xhr2 ? this._xhr2[prop] : this['_' + prop]),
          set: (v) => {
            if (this._async && this._xhr2) this._xhr2[prop] = v;
            else this['_' + prop] = v;
          },
        });
      });
    }

    _ensureXhr2() {
      if (this._xhr2) return this._xhr2;
      if (!XHR2) throw new Error('async XMLHttpRequest requires the optional xhr2 package');
      this._xhr2 = new XHR2();
      return this._xhr2;
    }

    open(method, url, async = true) {
      this._method = method;
      this._url = url;
      this._async = async;
      if (async) this._ensureXhr2().open(method, url, async);
      else this._readyState = 1;
    }

    setRequestHeader(name, value) {
      this._headers[name] = value;
      if (this._async) this._ensureXhr2().setRequestHeader(name, value);
    }

    getResponseHeader(name) {
      if (this._async) return this._ensureXhr2().getResponseHeader(name);
      return this._syncResponseHeaders?.[name.toLowerCase()] || null;
    }

    getAllResponseHeaders() {
      if (this._async) return this._ensureXhr2().getAllResponseHeaders();
      if (!this._syncResponseHeaders) return '';
      return Object.entries(this._syncResponseHeaders)
        .map(([k, v]) => `${k}: ${v}`)
        .join('\r\n') + '\r\n';
    }

    send(body = null) {
      if (this._async) {
        this._ensureXhr2().send(body);
        return;
      }
      try {
        const response = syncFetch(this._url, { method: this._method, headers: this._headers, body });
        this._status = response.status;
        this._statusText = response.status >= 200 && response.status < 300 ? 'OK' : 'Error';
        this._responseURL = this._url;
        this._readyState = 4;
        this._syncResponseHeaders = response.headers || {};
        const arrayBuffer = new ArrayBuffer(response.bodyBytes.byteLength);
        new Uint8Array(arrayBuffer).set(response.bodyBytes);
        this._response = arrayBuffer;
        this._responseText = '';
        if (this._onreadystatechange) this._onreadystatechange();
        if (this._onload) this._onload();
      } catch (err) {
        this._status = 0;
        this._readyState = 4;
        if (this._onerror) this._onerror(err);
        if (this._onreadystatechange) this._onreadystatechange();
      }
    }

    abort() {
      if (this._async && this._xhr2) this._xhr2.abort();
      else this._readyState = 0;
    }
  };
}

export async function installXhrPolyfill(opts = {}) {
  const { syncFetch } = opts;
  if (!isNode || typeof globalThis.XMLHttpRequest !== 'undefined') return;
  let XHR2 = null;
  try {
    const { createRequire } = await import('module');
    const require = createRequire(import.meta.url);
    XHR2 = require('xhr2');
  } catch (_) {
    // Without xhr2 there is no async XHR. The synchronous path, which wasm
    // libraries use, still works.
  }
  globalThis.XMLHttpRequest = makeXhrClass(syncFetch, XHR2);
}

// Releases one consumer reference and terminates the worker at the last one.
// Returns true if this call terminated the worker. Call it once for each
// createSyncFetch: an extra call releases a reference of another consumer.
export async function shutdown() {
  refCount = Math.max(0, refCount - 1);
  if (refCount > 0 || !workerState) return false;
  const state = workerState;
  state.dead = true;
  workerState = null;
  await state.worker.terminate();
  return true;
}

// Stock wirings for a handler whose init called createSyncFetch. Each releases
// one reference. Put shutdownMethod in the handler methods as `shutdown`, for a
// client that closes the transport. Export moduleDestroy as the handler
// module's `destroy`, which worker-router calls once per worker at pool
// termination. A missing call does not hang exit, because terminate() reaps
// the nested fetch worker.
export const shutdownMethod = async () => {
  await shutdown();
  return { ok: true };
};

export const moduleDestroy = async () => {
  await shutdown();
};
