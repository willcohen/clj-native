// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Node worker thread that serves synchronous HTTP for a wasm library. Node has
// no synchronous HTTP, so the caller blocks on Atomics.wait while this worker
// runs an async fetch. A request is JSON {url, method, headers, body}.
//
// Three SharedArrayBuffers carry the traffic:
//   controlBuffer (12B, Int32Array)  [0]=request ready (1) / shutdown (2),
//                                    [1]=response ready (1), [2]=request generation
//   metaBuffer    (20B, Int32Array)  [status, bodyLength, headersLength, flags,
//                                     generation answered]
//   dataBuffer    (NB,  Uint8Array)  request bytes out, response bytes in
// Bit 0 of metaBuffer[3] is OVERFLOW_FLAG: the response did not fit dataBuffer.
//
// The timeout is an idle deadline, requestTimeoutMs (35s default), that each
// body chunk resets. A stall aborts the request with status 0. A slow transfer
// can outlast the caller's fixed wait. writeResponse then drops the response,
// because its generation is stale.
//
// doFetch follows redirects, like the JVM and browser transports. fetch drops
// Authorization and Cookie on a cross-origin redirect but keeps a custom
// header. A decorator that must authenticate a cross-origin target therefore
// uses a custom header.
//
// decorateUrl in the init message names a module whose default (or `decorate`)
// export is (request) => request, or a Promise of one. It runs here, away from
// the blocked caller, and a token refresh can await. If the import fails or
// exports no function, the worker posts an error, not ready.

import { parentPort } from 'worker_threads';

const OVERFLOW_FLAG = 1;
const DEFAULT_REQUEST_TIMEOUT_MS = 35000;

let controlBuffer = null;
let dataBuffer = null;
let metaBuffer = null;
let decorate = null;
let requestTimeoutMs = DEFAULT_REQUEST_TIMEOUT_MS;
let shouldShutdown = false;

parentPort.on('message', async (msg) => {
  if (msg.cmd === 'init') {
    await handleInit(msg);
  } else if (msg.cmd === 'shutdown') {
    shouldShutdown = true;
    if (controlBuffer) {
      Atomics.store(controlBuffer, 0, 2);  // 2 = shutdown signal
      Atomics.notify(controlBuffer, 0, 1);
    }
    parentPort.postMessage({ status: 'shutdown_complete' });
  }
});

async function handleInit(msg) {
  controlBuffer = new Int32Array(msg.controlBuffer);
  dataBuffer = new Uint8Array(msg.dataBuffer);
  metaBuffer = new Int32Array(msg.metaBuffer);
  requestTimeoutMs = msg.requestTimeoutMs ?? DEFAULT_REQUEST_TIMEOUT_MS;
  if (msg.decorateUrl) {
    try {
      decorate = await importDecorator(msg.decorateUrl);
    } catch (err) {
      // Serving with no decorator would drop auth silently.
      parentPort.postMessage({ status: 'error', error: `decorate import failed: ${err.message}` });
      return;
    }
  }
  parentPort.postMessage({ status: 'ready' });
  pollForRequests();
}

async function importDecorator(decorateUrl) {
  const mod = await import(decorateUrl);
  const fn = mod.default ?? mod.decorate ?? null;
  if (typeof fn !== 'function') {
    throw new Error(`module ${decorateUrl} exports no default/decorate function`);
  }
  return fn;
}

async function pollForRequests() {
  while (!shouldShutdown) {
    Atomics.wait(controlBuffer, 0, 0, 100);  // Wake every 100ms, so a shutdown gets a fast answer.
    const signal = Atomics.load(controlBuffer, 0);
    if (signal === 2 || shouldShutdown) break;
    if (signal === 1) await handleRequest();
  }
}

async function handleRequest() {
  // Read now. A caller that gives up moves it on.
  const generation = Atomics.load(controlBuffer, 2);
  const controller = new AbortController();
  let timer = null;
  // Idle timer. doFetch resets it on each body chunk.
  const armIdle = () => {
    if (timer) clearTimeout(timer);
    timer = setTimeout(() => controller.abort(new Error('request timeout')), requestTimeoutMs);
  };
  const disarmIdle = () => { if (timer) { clearTimeout(timer); timer = null; } };
  armIdle();
  try {
    const request = readRequest();
    const decorated = await applyDecorator(request, controller.signal);
    const response = await doFetch(decorated, controller.signal, armIdle);
    writeResponse(response, generation);
  } catch (_err) {
    // Status 0 releases the blocked caller.
    writeResponse({ status: 0, body: new Uint8Array(0), headers: '' }, generation);
  } finally {
    disarmIdle();
  }
}

// Request wire format: [jsonLength:4 LE][json utf8].
function readRequest() {
  const view = new DataView(dataBuffer.buffer);
  const jsonLength = view.getInt32(0, true);
  const jsonBytes = dataBuffer.slice(4, 4 + jsonLength);
  return JSON.parse(new TextDecoder().decode(jsonBytes));
}

// The decorator is user code that can hang. The race against the abort signal
// keeps a hang from blocking this worker and its caller forever.
function applyDecorator(request, signal) {
  if (!decorate) return Promise.resolve(request);
  return Promise.race([Promise.resolve(decorate(request)), abortSignalPromise(signal)]);
}

function abortSignalPromise(signal) {
  return new Promise((_, reject) => {
    if (signal.aborted) { reject(signal.reason); return; }
    signal.addEventListener('abort', () => reject(signal.reason), { once: true });
  });
}

async function doFetch(request, signal, rearmIdle) {
  const { url, method = 'GET', headers = {}, body = null } = request;
  // fetch uppercases the method and then rejects a body on GET and HEAD.
  // Uppercase here to drop the body of a lowercase 'get' instead of failing.
  const m = String(method).toUpperCase();
  const init = { method: m, headers, redirect: 'follow', signal };
  if (body != null && m !== 'GET' && m !== 'HEAD') init.body = body;
  const res = await fetch(url, init);
  const chunks = [];
  if (res.body) {
    for await (const chunk of res.body) {
      rearmIdle();
      chunks.push(chunk);
    }
  }
  const buf = Buffer.concat(chunks);
  const headersStr = [...res.headers].map(([k, v]) => `${k}: ${v}`).join('\r\n');
  return {
    status: res.status,
    body: new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength),
    headers: headersStr,
  };
}

// Writes the body at dataBuffer[0] and the headers after it. Writes all data
// before control[1]=1 and the notify, to wake the caller to a consistent
// snapshot. If body plus headers exceed dataBuffer, writes no body and sets
// OVERFLOW_FLAG, because a silent truncation would corrupt a large binary read.
function writeResponse(response, generation) {
  // Stale: the caller gave up and can have a new request staged in
  // dataBuffer. The return comes before the control[0] clear, and the poll
  // loop still sees the new request.
  if (Atomics.load(controlBuffer, 2) !== generation) return;

  const headersBytes = new TextEncoder().encode(response.headers);
  const total = response.body.length + headersBytes.length;
  if (total > dataBuffer.length) {
    console.error(
      `fetch_worker: response (${response.body.length}B body + ${headersBytes.length}B headers) ` +
      `does not fit the ${dataBuffer.length}B transport buffer. Overflow flag set.`);
    Atomics.store(metaBuffer, 0, response.status);  // Upstream status, for diagnosis.
    Atomics.store(metaBuffer, 1, 0);
    Atomics.store(metaBuffer, 2, 0);
    Atomics.store(metaBuffer, 3, OVERFLOW_FLAG);
  } else {
    Atomics.store(metaBuffer, 0, response.status);
    Atomics.store(metaBuffer, 1, response.body.length);
    Atomics.store(metaBuffer, 2, headersBytes.length);
    Atomics.store(metaBuffer, 3, 0);
    dataBuffer.set(response.body, 0);
    dataBuffer.set(headersBytes, response.body.length);
  }
  Atomics.store(metaBuffer, 4, generation);
  // A caller that times out between the generation check and this clear can
  // lose its next request, which then waits out its budget. See the stale
  // branch in nodeSyncFetch.
  Atomics.store(controlBuffer, 0, 0);
  Atomics.store(controlBuffer, 1, 1);
  Atomics.notify(controlBuffer, 1, 1);
}
