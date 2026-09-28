// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Wraps a methods object into a ModuleHandler async factory. All methods of
// one handler run serially on one queue. A destroy starts only after every
// earlier call ends. The caller gets the real result, rejection included.
//
// Because the queue is serial, inFlight never exceeds 1 and awaitDrain()
// never waits. The busy/destroy split stays for a queue that serializes
// entry only.
//
// Logging is off by default and costs one check per call. setLogConfig({level,
// categories}) turns it on. Levels: off, error, warn, info, debug, trace. A
// category is the lowercase tag prefix before the first '-' ('BUSY-INC' gives
// 'busy'). Events go to a 256-entry ring that flushes to stderr on an uncaught
// error or an unhandled rejection. A worker gets its settings from
// initArgs.handlerRuntime {logLevel, logCategories}.

const LEVEL_RANK = { off: 0, error: 1, warn: 2, info: 3, debug: 4, trace: 5 };
const VALID_LEVELS = Object.keys(LEVEL_RANK);

// A null level means off. A null categories lets every category pass.
const logState = { level: null, categories: null, levelRank: 0 };

export const setLogConfig = (cfg) => {
  if (cfg === null || cfg === undefined) {
    logState.level = null;
    logState.categories = null;
    logState.levelRank = 0;
    return;
  }
  if (typeof cfg !== 'object') {
    throw new Error('setLogConfig: cfg must be an object or null');
  }
  // Validate both fields before either write, so a throw changes nothing.
  const hasLevel = Object.prototype.hasOwnProperty.call(cfg, 'level');
  let nextLevel = null;
  let nextRank = 0;
  if (hasLevel) {
    const lvl = cfg.level;
    if (lvl === null || lvl === undefined) {
      nextLevel = null;
      nextRank = 0;
    } else if (typeof lvl === 'string' && Object.prototype.hasOwnProperty.call(LEVEL_RANK, lvl)) {
      nextLevel = lvl;
      nextRank = LEVEL_RANK[lvl];
    } else {
      throw new Error('setLogConfig: level must be one of ' + VALID_LEVELS.join(', ') + ' or null (got ' + String(lvl) + ')');
    }
  }
  const hasCategories = Object.prototype.hasOwnProperty.call(cfg, 'categories');
  let nextCategories = null;
  if (hasCategories) {
    const cats = cfg.categories;
    if (cats === null || cats === undefined) {
      nextCategories = null;
    } else if (Array.isArray(cats) || cats instanceof Set) {
      const out = new Set();
      for (const c of cats) out.add(String(c).toLowerCase());
      nextCategories = out;
    } else {
      throw new Error('setLogConfig: categories must be an array, Set, or null');
    }
  }
  if (hasLevel) {
    logState.level = nextLevel;
    logState.levelRank = nextRank;
  }
  if (hasCategories) logState.categories = nextCategories;
};

export const getLogConfig = () => ({
  level: logState.level,
  categories: logState.categories ? Array.from(logState.categories) : null,
});

const RING_SIZE = 256;
const ringBuffer = new Array(RING_SIZE);
let ringIdx = 0;
let flushHandlersRegistered = false;

const categoryOf = (tag) => {
  const dash = tag.indexOf('-');
  return (dash > 0 ? tag.slice(0, dash) : tag).toLowerCase();
};

const isEventEnabled = (tag, eventLevel) => {
  if (logState.levelRank === 0) return false;
  const evRank = LEVEL_RANK[eventLevel] ?? LEVEL_RANK.debug;
  if (evRank > logState.levelRank) return false;
  if (logState.categories === null) return true;
  return logState.categories.has(categoryOf(tag));
};

// Date.now(), because performance.now() has a different origin in each worker
// and cannot order events across workers.
const formatEvent = (tag, fields) => {
  const ts = String(Date.now());
  let line = '[CLJ-NATIVE ts=' + ts + ' ' + tag + ']';
  if (fields) {
    for (const k of Object.keys(fields)) {
      const v = fields[k];
      if (v !== undefined && v !== null) line += ' ' + k + '=' + v;
    }
  }
  return line;
};

const dbgEmit = (tag, fields) => {
  const line = formatEvent(tag, fields);
  ringBuffer[ringIdx % RING_SIZE] = line;
  ringIdx++;
  console.log(line);
};

export const dbg = (tag, fields, level = 'debug') => {
  if (!isEventEnabled(tag, level)) return;
  dbgEmit(tag, fields);
};

// Take one isEnabled() snapshot at the opening event of a start/end pair and
// pass it to dbgPaired for the closing event. The two events then emit
// together, even if setLogConfig changes during the call.
export const isEnabled = (tag, level = 'debug') => isEventEnabled(tag, level);

export const dbgPaired = (enabled, tag, fields) => {
  if (!enabled) return;
  dbgEmit(tag, fields);
};

export const flushDebugRing = (reason) => {
  if (logState.levelRank === 0) return;
  const tag = reason ? String(reason) : 'flush';
  const total = Math.min(ringIdx, RING_SIZE);
  const start = ringIdx > RING_SIZE ? (ringIdx % RING_SIZE) : 0;
  console.error('=== CLJ-NATIVE ring flush (' + tag + ', last ' + total + ' events) ===');
  for (let i = 0; i < total; i++) {
    console.error(ringBuffer[(start + i) % RING_SIZE]);
  }
  console.error('=== end CLJ-NATIVE ring (' + tag + ') ===');
};

// The monitor keeps Node's default exit, so a crashed pool worker still ends,
// and it also sees an unhandled rejection. An uncaughtException listener
// would replace the exit.
const registerFlushHandlers = () => {
  if (flushHandlersRegistered) return;
  if (typeof process !== 'undefined' && typeof process.on === 'function') {
    process.on('uncaughtExceptionMonitor', (e, origin) => {
      flushDebugRing(origin + ': ' + (e && e.message ? e.message : String(e)));
    });
    flushHandlersRegistered = true;
  }
};
registerFlushHandlers();

// A typed array, an ArrayBuffer or a Buffer has a numeric byteLength. A plain
// array has none, and its contents stay in the fingerprint.
const byteCarrierLength = (v) =>
  (v !== null && typeof v === 'object' && typeof v.byteLength === 'number') ? v.byteLength : null;

// A Date, a class instance or another non-plain object goes to JSON.stringify
// as it is.
const isWalkable = (v) => {
  if (v === null || typeof v !== 'object') return false;
  if (Array.isArray(v)) return true;
  const proto = Object.getPrototypeOf(v);
  return proto === Object.prototype || proto === null;
};

// Replace each typed array, ArrayBuffer or Buffer with a length token before
// stringify. A replacer cannot do this, because Buffer.toJSON runs before the
// replacer sees the Buffer. `depth` bounds the walk against a cycle.
const withoutBytes = (value, depth) => {
  // JSON.stringify throws on a BigInt. A wasm init payload can hold them for
  // 64-bit sizes and pointers.
  if (typeof value === 'bigint') return '<bigint:' + value + '>';
  const n = byteCarrierLength(value);
  if (n !== null) return '<bytes:' + n + '>';
  if (!isWalkable(value) || depth <= 0) return value;
  if (Array.isArray(value)) return value.map((v) => withoutBytes(v, depth - 1));
  const out = {};
  for (const k of Object.keys(value)) out[k] = withoutBytes(value[k], depth - 1);
  return out;
};

const FINGERPRINT_DEPTH = 4;

// JSON.stringify alone writes a typed array as megabytes of text and an
// ArrayBuffer as `{}`.
const defaultFingerprint = (args) => {
  try { return JSON.stringify(withoutBytes(args ?? null, FINGERPRINT_DEPTH) ?? null); }
  catch (e) {
    // A String(args) fallback would make every such payload equal.
    throw new Error(
      'makeHandler: init args are not fingerprintable ('
      + (e && e.message ? e.message : String(e))
      + '). Pass an explicit `fingerprint`, for example byteLengthFingerprint([...]).',
      { cause: e },
    );
  }
};

// A fingerprint over the named init-arg fields only, with the same bytes rule.
// Use it when the handler must ignore an init arg that can change while the
// loaded module stays the same. A named plain array counts by its length.
// `prefix` labels the handler in the re-init error.
export const byteLengthFingerprint = (fields, prefix = null) => {
  if (!Array.isArray(fields) || fields.length === 0) {
    throw new Error('byteLengthFingerprint: fields must be a non-empty array of init-arg names');
  }
  return (initArgs) => {
    const args = initArgs ?? {};
    const parts = fields.map((k) => {
      const v = args[k];
      const n = byteCarrierLength(v) ?? (Array.isArray(v) ? v.length : null);
      return k + ':' + (n === null ? String(v) : n);
    });
    return prefix ? prefix + '|' + parts.join('|') : parts.join('|');
  };
};

// A wasm RuntimeError or an emscripten `Aborted(...)` error does not cross
// comlink RPC cleanly: structuredClone can fail, or the caller's await can
// stall. Returns a plain Error with `wasmTrap: true` and a `kind` for those,
// and any other error unchanged.
export const normalizeWasmError = (e) => {
  let kind;
  if (typeof WebAssembly !== 'undefined'
      && WebAssembly.RuntimeError
      && e instanceof WebAssembly.RuntimeError) {
    kind = 'wasm-runtime-error';
  } else if (e instanceof Error
             && typeof e.message === 'string'
             && e.message.startsWith('Aborted')) {
    kind = 'emscripten-abort';
  } else {
    return e;
  }
  const wrapped = new Error(e?.message ?? String(e));
  wrapped.wasmTrap = true;
  wrapped.kind = kind;
  if (e?.stack) wrapped.stack = e.stack;
  return wrapped;
};

export function makeHandler({
  init,
  fingerprint,
  methods,
  busyMethods = [],
  destroyMethods = [],
  label = null,
} = {}) {
  // If init calls ctx.attachEmscriptenModule(module), BUSY events carry
  // heap-bytes and brk.
  let emscriptenModule = null;
  const inspectHeap = () => {
    const m = emscriptenModule;
    if (!m || !m.HEAPU8) return null;
    const out = { 'heap-bytes': m.HEAPU8.length };
    if (typeof m._sbrk === 'function') {
      try { out.brk = m._sbrk(0); } catch (_e) { /* ignore */ }
    }
    return out;
  };
  // The second argument to init. It lives as long as the handler, so init can
  // keep it. A consumer must take the log functions from here, not from an
  // import: a bundled handler and its external overrides module can load two
  // copies of this module, each with its own logState.
  const substrateCtx = {
    attachEmscriptenModule(m) { emscriptenModule = m; },
    dbg,
    isEnabled,
    dbgPaired,
    getLogConfig,
  };
  if (!methods || typeof methods !== 'object' || Object.keys(methods).length === 0) {
    throw new Error('makeHandler: `methods` is required and must be a non-empty object');
  }
  for (const name of busyMethods) {
    if (!Object.prototype.hasOwnProperty.call(methods, name)) {
      throw new Error(`makeHandler: busyMethod '${name}' not present in methods`);
    }
  }
  for (const name of destroyMethods) {
    if (!Object.prototype.hasOwnProperty.call(methods, name)) {
      throw new Error(`makeHandler: destroyMethod '${name}' not present in methods`);
    }
  }
  const fp = fingerprint ?? defaultFingerprint;
  const busy = new Set(busyMethods);
  const destroy = new Set(destroyMethods);
  // wrap() tests destroy first, and a name in both sets would lose its busy
  // accounting with no error.
  for (const name of busy) {
    if (destroy.has(name)) {
      throw new Error(`makeHandler: method '${name}' is in both busyMethods and destroyMethods`);
    }
  }

  let workerQueue = Promise.resolve();
  let inFlight = 0;
  // The `id` on BUSY and DESTROY events, to pair start and end events.
  let callIdCounter = 0;
  // Set by __setWorkerSlot. A handler outside a pool keeps null and emits no
  // slot field.
  let runtimeSlot = null;
  // Each resolves the next time inFlight reaches 0.
  let barrierResolvers = [];
  const releaseBarriers = () => {
    if (inFlight === 0 && barrierResolvers.length > 0) {
      const pending = barrierResolvers;
      barrierResolvers = [];
      for (const r of pending) r();
    }
  };
  const awaitDrain = () => {
    if (inFlight === 0) return Promise.resolve();
    return new Promise((resolve) => { barrierResolvers.push(resolve); });
  };

  const wrap = (name, fn) => {
    if (destroy.has(name)) {
      return (...args) => {
        const next = workerQueue.then(async () => {
          await awaitDrain();
          // One snapshot, so COMPLETE always pairs with FIRE.
          const enabled = isEventEnabled('DESTROY-FIRE', 'debug');
          let id;
          if (enabled) {
            id = ++callIdCounter;
            dbgPaired(true, 'DESTROY-FIRE', { fn: name, label, slot: runtimeSlot, busy: inFlight, id });
          }
          try {
            return await fn(...args);
          } catch (e) {
            throw normalizeWasmError(e);
          } finally {
            if (enabled) {
              dbgPaired(true, 'DESTROY-COMPLETE', { fn: name, label, slot: runtimeSlot, id });
            }
          }
        });
        workerQueue = next.catch(() => {});
        return next;
      };
    }
    if (busy.has(name)) {
      return (...args) => {
        const next = workerQueue.then(async () => {
          inFlight++;
          // One snapshot, so DEC always pairs with INC. inspectHeap() calls
          // into wasm, and it runs only when enabled.
          const enabled = isEventEnabled('BUSY-INC', 'debug');
          let id, cFn;
          if (enabled) {
            id = ++callIdCounter;
            cFn = (name === 'ccall' && args.length > 0) ? args[0] : null;
            dbgPaired(true, 'BUSY-INC', { fn: name, 'c-fn': cFn, label, slot: runtimeSlot, counter: inFlight, id, ...(inspectHeap() || {}) });
          }
          try {
            return await fn(...args);
          } catch (e) {
            throw normalizeWasmError(e);
          } finally {
            inFlight--;
            if (enabled) {
              dbgPaired(true, 'BUSY-DEC', { fn: name, 'c-fn': cFn, label, slot: runtimeSlot, counter: inFlight, id, ...(inspectHeap() || {}) });
            }
            releaseBarriers();
          }
        });
        workerQueue = next.catch(() => {});
        return next;
      };
    }
    return (...args) => {
      const next = workerQueue.then(async () => {
        try { return await fn(...args); }
        catch (e) { throw normalizeWasmError(e); }
      });
      workerQueue = next.catch(() => {});
      return next;
    };
  };

  let cachedFingerprint = null;
  let cachedHandler = null;
  let initPromise = null;

  const factory = async (initArgs) => {
    // A worker is a separate JS context that a host setLogConfig cannot
    // reach.
    if (initArgs && initArgs.handlerRuntime) {
      const hr = initArgs.handlerRuntime;
      setLogConfig({ level: hr.logLevel, categories: hr.logCategories });
    }
    const print = fp(initArgs);
    if (cachedHandler !== null) {
      if (print !== cachedFingerprint) {
        throw new Error(
          `makeHandler: re-init with different args (cached fingerprint ${cachedFingerprint}, got ${print})`,
        );
      }
      return cachedHandler;
    }
    if (initPromise === null) {
      cachedFingerprint = print;
      initPromise = (async () => {
        try {
          if (typeof init === 'function') await init(initArgs, substrateCtx);
          const wrapped = {};
          for (const name of Object.keys(methods)) {
            wrapped[name] = wrap(name, methods[name]);
          }
          // Reserved name. The :handler-runtime broadcast of init-pool! calls
          // it. It bypasses wrap(), so it does not wait behind in-flight calls.
          wrapped.__setLogConfig = (cfg) => setLogConfig(cfg);
          // Reserved name. init-pool! calls it once per worker, and later
          // events carry `slot=N`. It records the slot while logging is off
          // too.
          wrapped.__setWorkerSlot = (s) => { runtimeSlot = s; };
          cachedHandler = wrapped;
          return wrapped;
        } catch (e) {
          // Roll back to let the caller retry with corrected args.
          cachedFingerprint = null;
          cachedHandler = null;
          initPromise = null;
          throw e;
        }
      })();
    } else if (print !== cachedFingerprint) {
      throw new Error(
        `makeHandler: re-init with different args while init in flight (pending fingerprint ${cachedFingerprint}, got ${print})`,
      );
    }
    return initPromise;
  };

  return factory;
}

export default makeHandler;
