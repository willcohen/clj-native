// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: an HTTP echo server in its own worker thread, since syncFetch
// blocks the calling thread in Atomics.wait.
import { parentPort } from 'node:worker_threads';
import http from 'node:http';

const redirects = { '/redirect-chain': '/redirect-chain-2', '/redirect-chain-2': '/query?redirected=2' };

const server = http.createServer((req, res) => {
  const chunks = [];
  req.on('data', (c) => chunks.push(c));
  req.on('end', () => {
    const location = redirects[req.url];
    if (location) {
      res.setHeader('location', location);
      res.statusCode = 302;
      res.end();
      return;
    }
    if (req.url.startsWith('/empty')) {
      const pad = Number(new URL(req.url, 'http://x').searchParams.get('pad')) || 0;
      res.setHeader('x-pad', 'p'.repeat(pad));
      res.statusCode = 204;
      res.end();
      return;
    }
    // `chunks` chunks of `size` bytes, `delay` ms apart: each gap stays under
    // requestTimeoutMs while the total goes over it.
    if (req.url.startsWith('/slow')) {
      const u = new URL(req.url, 'http://x');
      const chunks = Number(u.searchParams.get('chunks')) || 3;
      const size = Number(u.searchParams.get('size')) || 10;
      const delay = Number(u.searchParams.get('delay')) || 50;
      res.setHeader('content-type', 'application/octet-stream');
      res.statusCode = 200;
      let i = 0;
      const tick = () => {
        if (i >= chunks) { res.end(); return; }
        res.write(Buffer.alloc(size, 0x79));  // 0x79 = 'y'
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
