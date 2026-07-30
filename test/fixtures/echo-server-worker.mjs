// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: an HTTP echo server running in its own worker thread. The
// http-bridge's syncFetch blocks its calling thread on Atomics.wait; if the
// fixture server shared that event loop it could never accept the fetch
// worker's connection. Hosting it in a separate thread mirrors production,
// where the sync caller is an emscripten pthread and the server is external.
import { parentPort } from 'node:worker_threads';
import http from 'node:http';

const server = http.createServer((req, res) => {
  const chunks = [];
  req.on('data', (c) => chunks.push(c));
  req.on('end', () => {
    // Redirect fixtures exercise redirect:'follow'. Check the more specific
    // chain paths before the generic /redirect (startsWith would match both).
    // /redirect-chain -> /redirect-chain-2 -> /query?redirected=2 (two 302 hops).
    if (req.url.startsWith('/redirect-chain-2')) {
      res.setHeader('location', '/query?redirected=2');
      res.statusCode = 302;
      res.end();
      return;
    }
    if (req.url.startsWith('/redirect-chain')) {
      res.setHeader('location', '/redirect-chain-2');
      res.statusCode = 302;
      res.end();
      return;
    }
    // /redirect -> single 302 to a path that echoes its own url, letting the
    // test confirm the final response is the target, not the empty 3xx.
    if (req.url.startsWith('/redirect')) {
      res.setHeader('location', '/query?redirected=1');
      res.statusCode = 302;
      res.end();
      return;
    }
    // /large?bytes=N -> N raw bytes, to drive the transport-buffer overflow path.
    if (req.url.startsWith('/large')) {
      const n = Number(new URL(req.url, 'http://x').searchParams.get('bytes')) || 0;
      res.setHeader('content-type', 'application/octet-stream');
      res.statusCode = 200;
      res.end(Buffer.alloc(n, 0x78));  // 0x78 = 'x'
      return;
    }
    // /slow?chunks=N&delay=M -> N 10-byte chunks M ms apart, so total transfer
    // time exceeds a short requestTimeoutMs while each gap stays under it. Proves
    // the worker's timeout is an idle timer (reset per chunk), not a whole-request
    // cap that would abort a healthy slow transfer.
    if (req.url.startsWith('/slow')) {
      const u = new URL(req.url, 'http://x');
      const chunks = Number(u.searchParams.get('chunks')) || 3;
      const delay = Number(u.searchParams.get('delay')) || 50;
      res.setHeader('content-type', 'application/octet-stream');
      res.statusCode = 200;
      let i = 0;
      const tick = () => {
        if (i >= chunks) { res.end(); return; }
        res.write(Buffer.alloc(10, 0x79));  // 0x79 = 'y'
        i += 1;
        setTimeout(tick, delay);
      };
      tick();
      return;
    }
    res.setHeader('content-type', 'application/json');
    res.setHeader('x-fixture', 'yes');
    res.statusCode = 200;
    res.end(JSON.stringify({
      url: req.url,
      method: req.method,
      headers: req.headers,
      body: Buffer.concat(chunks).toString(),
    }));
  });
});

server.listen(0, '127.0.0.1', () => {
  parentPort.postMessage({ type: 'ready', port: server.address().port });
});

parentPort.on('message', (msg) => {
  if (msg?.cmd === 'close') {
    server.close(() => parentPort.postMessage({ type: 'closed' }));
  }
});
