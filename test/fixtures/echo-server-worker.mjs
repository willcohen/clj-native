// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Test fixture: an HTTP echo server in its own worker thread, since syncFetch
// blocks the calling thread in Atomics.wait.
import { parentPort } from 'node:worker_threads';
import http from 'node:http';

const server = http.createServer((req, res) => {
  const chunks = [];
  req.on('data', (c) => chunks.push(c));
  req.on('end', () => {
    // Match the chain paths before /redirect, since startsWith matches both.
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
    if (req.url.startsWith('/redirect')) {
      res.setHeader('location', '/query?redirected=1');
      res.statusCode = 302;
      res.end();
      return;
    }
    if (req.url.startsWith('/large')) {
      const n = Number(new URL(req.url, 'http://x').searchParams.get('bytes')) || 0;
      res.setHeader('content-type', 'application/octet-stream');
      res.statusCode = 200;
      res.end(Buffer.alloc(n, 0x78));  // 0x78 = 'x'
      return;
    }
    // N 10-byte chunks M ms apart: each gap stays under requestTimeoutMs while
    // the total goes over it.
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
