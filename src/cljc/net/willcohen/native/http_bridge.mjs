// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Platform HTTP transport for wasm libraries whose C runtime issues blocking
// XHR, for example emscripten FETCH or the HTTP entry point of a library.
// There is one bridge for each process.
//
// createSyncFetch(opts) returns syncFetch(url, reqOpts), which returns
// {status, headers, bodyBytes}. On Node the call goes to a fetch_worker over
// SharedArrayBuffer and Atomics. In a browser it is a synchronous XHR, and the
// caller must already be in a Web Worker. createSyncFetch returns a Promise,
// because on Node it waits for the worker to become ready.
//
// Four behaviors here are safety-critical:
//   - On Node, createSyncFetch REJECTS if the worker cannot become ready. A
//     decorator import failure is one such case. The alternative would serve
//     requests with no authentication.
//   - A response larger than dataBufferSize resolves to {status:0,
//     overflow:true}, which a caller can tell apart from a network error.
//   - A worker that stops answering unblocks the caller with status 0 after
//     requestTimeoutMs plus slack, in place of a permanent Atomics.wait.
//   - Every request carries a generation, so a response that the caller
//     already gave up on can never become the answer to the next request.
//
// The auth decorator runs in the worker on Node, and synchronously in line in
// a browser. It never runs in the blocked caller.
//
// installXhrPolyfill sends the synchronous path of a global XHR through
// syncFetch. shutdown() releases one consumer reference, and the worker
// terminates at the last release. shutdownMethod and moduleDestroy are the two
// stock wirings that a generated handler needs. See them at the end of this
// file.

import { isNode } from './handler_env.mjs';

const CONTROL_BUFFER_SIZE = 12;
const META_BUFFER_SIZE = 20;
const DEFAULT_DATA_BUFFER_SIZE = 50 * 1024 * 1024;
const DEFAULT_REQUEST_TIMEOUT_MS = 35000;
// The caller waits a little longer than the request timeout of the worker.
// This is a backstop against an Atomics.wait that never ends.
//
// This cap does more than catch a crashed worker. An earlier version of this
// comment claimed that it did only that, and the claim hid a real defect. The
// timeout of the worker is an IDLE timer, and each body chunk resets it, so a
// healthy slow transfer has no total bound. This cap is fixed wall-clock time,
// and nothing resets it. Any transfer longer than the cap, with no single stall
// longer than requestTimeoutMs, ends here while the worker still streams. The
// generation on every request is what stops that abandoned response from
// becoming the answer to the next request.
const WAIT_SLACK_MS = 5000;
// How long ensureWorker waits for the worker's 'ready' before giving up.
const WORKER_READY_TIMEOUT_MS = 10000;
// metaBuffer[3] bit 0: the response did not fit the data buffer (see fetch_worker).
const OVERFLOW_FLAG = 1;
// The generation pairs a response with the request that asked for it. The wait
// of the caller is a wall-clock cap, but the timeout of the worker is an idle
// timer. A healthy slow transfer can therefore outlive the patience of the
// caller, and the worker continues to serve a request that nobody waits for.
// Without a generation, that abandoned response reached the shared buffers and
// became the answer to the NEXT request: status 200 with the bytes of another
// URL, which no downstream caller can detect. This function never issues 0, so
// a generation cannot collide with a metaBuffer that was just zeroed.
function nextGeneration(state) {
  const next = (state.generation + 1) | 0;
  state.generation = next === 0 ? 1 : next;
  return state.generation;
}

// There is one fetch worker for each process. workerState is a global
// singleton for the process, and ensureWorker creates it on first use.
// Consumers share it, because a joint pool loads the handler modules of
// several libraries into one worker thread. The ensure and shutdown pairs are
// therefore reference-counted. Each successful createSyncFetch on Node takes
// one reference, and shutdown() releases one. The worker terminates at the
// last release, and not at the teardown of the first consumer.
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

// Send the init message and resolve when the worker reports ready. The
// function rejects in four cases: the worker reports an init error, the worker
// emits an 'error', the worker exits during init, or the worker does not
// report ready within WORKER_READY_TIMEOUT_MS. A decorator import failure is
// one such init error, and it must never fall back to serving with no
// authentication. An earlier version used a busy-wait that could turn forever.
// On any rejection this function terminates the worker and leaves workerState
// null, so a later createSyncFetch starts clean.
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

  // One wall-clock budget, whatever number of stale responses arrive.
  const deadline = Date.now() + waitTimeoutMs;
  for (;;) {
    const remaining = deadline - Date.now();
    if (remaining <= 0 || Atomics.wait(control, 1, 0, remaining) === 'timed-out') {
      // The worker crashed, stopped, or still serves a transfer that outlived
      // this budget. Return a transport failure, so that the caller does not
      // hang forever. The worker drops that response when it arrives. See
      // writeResponse.
      console.error(
        `http-bridge: fetch worker did not answer within ${waitTimeoutMs}ms for ${url}. ` +
        'Returning a transport failure.');
      return { status: 0, headers: {}, bodyBytes: new Uint8Array(0) };
    }
    if (Atomics.load(meta, 4) === generation) break;
    // This is a response for a request that this caller already gave up on.
    // writeResponse drops such a response on the worker side, so this check
    // guards only the window between its generation test and its control[1]
    // store. That window holds two dataBuffer.set copies, so it gets wider as
    // the body gets larger. The suite cannot force that interleaving, so no
    // test covers this branch. It stays because the check on the worker side is
    // not atomic with its write.
    Atomics.store(control, 1, 0);
  }

  const flags = Atomics.load(meta, 3);
  const status = Atomics.load(meta, 0);
  if (flags & OVERFLOW_FLAG) {
    // A caller can tell this apart from a network error, which has status 0
    // and no overflow field. The upstream request was good, but its body was
    // larger than the transport buffer.
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
  if (decorate) request = decorate(request);  // The browser path is synchronous only.

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
      this._xhr2 = null;  // lazily built; only the async path needs xhr2
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
    // Without xhr2 there is no async XHR. The synchronous path, which is the
    // one that every wasm library uses, works in either case.
  }
  globalThis.XMLHttpRequest = makeXhrClass(syncFetch, XHR2);
}

// Release one consumer reference. The function returns true if this call
// terminated the worker, which happens at the last release, or when a live
// worker has no references left. It returns false if the worker stays up for
// other consumers, or if no worker was ever started. A call made after all
// references are released is a safe no-op.
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

// The two stock wirings for a handler whose init called createSyncFetch. Both
// release one reference. They differ only in which path reaches them.
//
// shutdownMethod goes in the methods object of the handler under the name
// `shutdown`, where an explicit client RPC reaches it. moduleDestroy is the
// top-level `destroy` export of the handler module. The coordinator in
// worker-router invokes that export one time for each worker when the pool
// terminates. See loadAllHandlers in worker-bootstrap. A handler needs both:
// the method for a client that closes the transport on purpose during a
// session, and the export for the teardown path that no client calls.
//
// A deliberate close is not a fix for a leak. This was measured on node 26
// with sixteen pool workers. Each one started a fetch worker, did a real round
// trip, and then terminated with no call to destroy. The process still exits
// 0. That holds even when a pool worker sits in Atomics.wait and its fetch
// worker is in the middle of a fetch, because terminate() reaps the nested
// thread. These exports were first written for the node 24 libuv regression,
// which the flakes now pin past.
export const shutdownMethod = async () => {
  await shutdown();
  return { ok: true };
};

export const moduleDestroy = async () => {
  await shutdown();
};
