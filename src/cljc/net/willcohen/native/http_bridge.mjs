// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// HTTP transport for a wasm library whose C runtime issues blocking XHR, such
// as emscripten FETCH. There is one bridge per thread.
//
// createSyncFetch(opts) resolves to syncFetch(url, reqOpts), which returns
// {status, headers, bodyBytes}. On Node, syncFetch blocks on Atomics while a
// fetch_worker does the request. In a browser it is a synchronous XHR, and the
// caller must be in a Web Worker.
//
// Contracts:
//   - On Node, createSyncFetch rejects if the worker cannot become ready, for
//     example when the decorator import fails. A later call reuses the
//     running worker and ignores its own decorateUrl, dataBufferSize and
//     requestTimeoutMs.
//   - A response larger than dataBufferSize gives {status: 0, overflow: true}.
//   - A worker that stops answering gives status 0 after requestTimeoutMs plus
//     slack.
//   - A response that the caller gave up on never answers the next request.
//
// The auth decorator runs in the worker on Node and inline in a browser, never
// in the blocked caller. installXhrPolyfill sends the synchronous path of a
// global XHR through syncFetch.

import { isNode } from './handler_env.mjs';

const CONTROL_BUFFER_SIZE = 12;
const META_BUFFER_SIZE = 20;
const DEFAULT_DATA_BUFFER_SIZE = 50 * 1024 * 1024;
const DEFAULT_REQUEST_TIMEOUT_MS = 35000;
// The caller waits past the worker's idle timeout, so a stall comes back as
// the worker's status 0. A slow transfer can still outlast this fixed cap, and
// the request generation keeps its late response off the next request.
const WAIT_SLACK_MS = 5000;
const WORKER_READY_TIMEOUT_MS = 10000;
// metaBuffer[3] bit 0: the response did not fit dataBuffer.
const OVERFLOW_FLAG = 1;
// Never returns 0, so a generation cannot match a zeroed metaBuffer.
function nextGeneration(state) {
  const next = (state.generation + 1) | 0;
  state.generation = next === 0 ? 1 : next;
  return state.generation;
}

// One fetch worker per thread, shared and reference-counted, because a joint
// pool loads the handlers of several libraries into one worker thread. Each
// createSyncFetch on Node takes a reference, and shutdown() releases one.
let workerState = null;
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

async function ensureWorker(workerUrl, decorateUrl, opts) {
  if (workerState) return workerState;
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
    decorateUrl: decorateUrl ? String(decorateUrl) : null,
    requestTimeoutMs,
  });

  workerState = {
    worker,
    control: new Int32Array(controlSAB),
    meta: new Int32Array(metaSAB),
    data: new Uint8Array(dataSAB),
    view: new DataView(dataSAB),
    waitTimeoutMs: requestTimeoutMs + WAIT_SLACK_MS,
    generation: 0,
  };
  return workerState;
}

// Resolves when the worker reports ready. Rejects on an init error, a worker
// 'error', a nonzero exit, or a timeout. A rejection terminates the worker and
// leaves workerState null, and a later createSyncFetch starts clean.
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
    const onExit = (code) => {
      if (code !== 0) {
        finishReject(new Error(`createSyncFetch: fetch worker exited (code ${code}) during init`));
      }
    };
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

function nodeSyncFetch(state, url, reqOpts) {
  const { control, meta, data, view, waitTimeoutMs } = state;
  const request = {
    url,
    method: reqOpts.method || 'GET',
    headers: reqOpts.headers || {},
    body: reqOpts.body ?? null,
  };
  const jsonBytes = new TextEncoder().encode(JSON.stringify(request));
  view.setInt32(0, jsonBytes.length, true);
  data.set(jsonBytes, 4);

  const generation = nextGeneration(state);
  Atomics.store(control, 2, generation);
  Atomics.store(control, 1, 0);
  Atomics.store(control, 0, 1);
  Atomics.notify(control, 0, 1);

  // One budget covers any number of stale responses.
  const deadline = Date.now() + waitTimeoutMs;
  for (;;) {
    const remaining = deadline - Date.now();
    if (remaining <= 0 || Atomics.wait(control, 1, 0, remaining) === 'timed-out') {
      // The worker crashed, stalled, or still streams a transfer that outlived
      // this budget. writeResponse drops that late response.
      console.error(
        `http-bridge: fetch worker did not answer within ${waitTimeoutMs}ms for ${url}. ` +
        'Returning a transport failure.');
      return { status: 0, headers: {}, bodyBytes: new Uint8Array(0) };
    }
    if (Atomics.load(meta, 4) === generation) break;
    // A stale response. writeResponse drops these, but its generation check is
    // not atomic with its write, and the gap holds two dataBuffer copies. No
    // test can force this interleaving.
    Atomics.store(control, 1, 0);
  }

  const flags = Atomics.load(meta, 3);
  const status = Atomics.load(meta, 0);
  if (flags & OVERFLOW_FLAG) {
    // `overflow` tells this apart from a network error.
    console.error(
      `http-bridge: the response for ${url} is larger than the ${data.length}-byte transport ` +
      `buffer. Upstream status ${status}. Returning a transport failure.`);
    return { status: 0, headers: {}, bodyBytes: new Uint8Array(0), overflow: true };
  }

  const bodyLength = Atomics.load(meta, 1);
  const headersLength = Atomics.load(meta, 2);
  const bodyBytes = data.slice(0, bodyLength);
  const headersStr = new TextDecoder().decode(data.slice(bodyLength, bodyLength + headersLength));
  return { status, headers: parseHeaders(headersStr), bodyBytes };
}

function browserSyncFetch(decorate, url, reqOpts) {
  let request = {
    url,
    method: reqOpts.method || 'GET',
    headers: reqOpts.headers || {},
    body: reqOpts.body ?? null,
  };
  if (decorate) request = decorate(request);  // A browser decorator must be synchronous.

  const xhr = new XMLHttpRequest();
  xhr.open(request.method, request.url, false);
  try { xhr.responseType = 'arraybuffer'; } catch (_) {}
  for (const [k, v] of Object.entries(request.headers)) xhr.setRequestHeader(k, v);
  xhr.send(request.body);

  let bodyBytes = new Uint8Array(0);
  if (xhr.response instanceof ArrayBuffer) {
    bodyBytes = new Uint8Array(xhr.response);
  } else if (typeof xhr.responseText === 'string' && xhr.responseText.length > 0) {
    bodyBytes = new TextEncoder().encode(xhr.responseText);
  }
  const all = xhr.getAllResponseHeaders ? xhr.getAllResponseHeaders() : '';
  return { status: xhr.status, headers: parseHeaders(all), bodyBytes };
}

export async function createSyncFetch(opts = {}) {
  const { workerUrl, decorate, decorateUrl } = opts;
  if (isNode) {
    const state = await ensureWorker(workerUrl, decorateUrl, opts);
    refCount++;
    return (url, reqOpts = {}) => nodeSyncFetch(state, url, reqOpts);
  }
  return (url, reqOpts = {}) => browserSyncFetch(decorate, url, reqOpts);
}

function makeXhrClass(syncFetch, XHR2) {
  let xhrIdCounter = 0;
  return class XMLHttpRequest {
    constructor() {
      this._id = ++xhrIdCounter;
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
// Returns true if this call terminated the worker. An extra call is safe.
export async function shutdown() {
  if (!workerState) {
    refCount = 0;
    return false;
  }
  refCount = Math.max(0, refCount - 1);
  if (refCount > 0) return false;
  const { worker } = workerState;
  worker.postMessage({ cmd: 'shutdown' });
  await worker.terminate();
  workerState = null;
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
