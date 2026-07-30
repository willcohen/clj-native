// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// fetch_worker.mjs. A Node worker thread that serves synchronous HTTP for a
// wasm library. Such a library has a C runtime that issues blocking XHR, for
// example emscripten FETCH or a synchronous fetch callback. Node has no
// synchronous HTTP, so the caller blocks on Atomics.wait while this worker does
// the async request. The request payload is JSON of the form
// {url, method, headers, body}, so one worker serves both range GETs and
// requests with any method and body.
//
// Three SharedArrayBuffers carry the traffic:
//   controlBuffer (12B, Int32Array)  [0]=request ready (1) / shutdown (2),
//                                    [1]=response ready (1), [2]=request generation
//   metaBuffer    (20B, Int32Array)  [status, bodyLength, headersLength, flags,
//                                     generation answered]
//   dataBuffer    (NB,  Uint8Array)  request bytes out, response bytes in
// Bit 0 of metaBuffer[3] is OVERFLOW_FLAG. It tells the caller that the response
// did not fit in dataBuffer. See writeResponse. The bridge makes dataBuffer
// 50MB by default.
//
// The generation pairs a response with the request that asked for it. The wait
// of the caller is a wall-clock cap, but the timeout below is an idle timer. A
// healthy slow transfer can therefore outlive the patience of the caller. In
// that case writeResponse drops the response, because its generation has moved
// on. Without the drop, the next request would receive the bytes of another URL.
//
// doFetch uses the global fetch with redirect:'follow', to agree with the JVM
// and browser transports. The earlier http/https.request path followed no
// redirect and returned an empty 3xx body on Node only. The global fetch drops
// Authorization and Cookie on a CROSS-ORIGIN 3xx hop, but a custom header
// stays. A decorator that authenticates a cross-origin redirect target must
// therefore use a custom header.
//
// The timeout is an idle deadline, requestTimeoutMs, 35s by default. Each body
// chunk resets it. A stall aborts the request to a status-0 response. A healthy
// slow transfer continues.
//
// The decorator seam carries auth. A decorateUrl in the init message names a
// module whose default export is (request) => request, or a Promise of one.
// This worker imports that module and applies the function here, away from the
// blocked caller, so a token refresh can await. If the import fails, or the
// export has the wrong shape, the worker refuses to report ready. To serve with
// decorate=null would drop auth without a word.

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
      // Refuse to report ready. To serve with decorate=null would drop auth
      // without a word.
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
    // Signal 0 means the wait timed out with no request. Continue to poll.
  }
}

async function handleRequest() {
  // The generation that this response answers. Read it before the caller can
  // move it on, which the caller does if it gives up and issues another request.
  const generation = Atomics.load(controlBuffer, 2);
  const controller = new AbortController();
  let timer = null;
  // An idle timer, not a cap on the whole request. Each body chunk resets it.
  // See doFetch. A slow but steady transfer therefore continues, and only a
  // stall of requestTimeoutMs aborts the request.
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
    // Any failure writes a status-0 response, so the blocked caller continues.
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

// The decorator is user code, and it can fail to resolve. Race it against the
// abort signal. A hang then aborts, instead of stopping both this worker and
// the caller that it blocks.
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
  // fetch makes the method uppercase and then refuses a body on GET and HEAD.
  // Make it uppercase here, so a lowercase 'get' with a body drops the body
  // instead of throwing and giving status 0.
  const m = String(method).toUpperCase();
  const init = { method: m, headers, redirect: 'follow', signal };
  if (body != null && m !== 'GET' && m !== 'HEAD') init.body = body;
  const res = await fetch(url, init);
  const chunks = [];
  if (res.body) {
    for await (const chunk of res.body) {
      rearmIdle();  // Progress resets the idle deadline, so a slow but steady transfer continues.
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

// Layout: the body bytes go at dataBuffer[0], and the headers go directly
// after them. meta is [status, bodyLength, headersLength, flags, generation].
// Write all of the data before control[1]=1 and the notify, so that the caller
// wakes to a consistent snapshot.
//
// Overflow: if the body and the headers together are larger than dataBuffer,
// write no body and set OVERFLOW. A silent truncation would corrupt a large
// binary read, and nobody would see it. A separate overflow status lets the
// caller act.
function writeResponse(response, generation) {
  // The caller gave up and moved on to another request. A write now would
  // destroy the request bytes that the caller has already staged, and would
  // give it the response for this URL in place of its own. Drop the response
  // instead, because it belongs to nobody. This return comes before the
  // control[0] clear below, so the pending request stays signalled and the
  // poll loop takes it on the next pass.
  if (Atomics.load(controlBuffer, 2) !== generation) return;

  const headersBytes = new TextEncoder().encode(response.headers);
  const total = response.body.length + headersBytes.length;
  if (total > dataBuffer.length) {
    console.error(
      `fetch_worker: response (${response.body.length}B body + ${headersBytes.length}B headers) ` +
      `does not fit the ${dataBuffer.length}B transport buffer. Overflow flag set.`);
    Atomics.store(metaBuffer, 0, response.status);  // The real upstream status, for diagnosis.
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
  // The clear is safe here only because a stale response returned above. A
  // caller that still waits on this generation is blocked in Atomics.wait, so
  // it cannot have signalled another request.
  Atomics.store(controlBuffer, 0, 0);
  Atomics.store(controlBuffer, 1, 1);
  Atomics.notify(controlBuffer, 1, 1);
}
