// Copyright (c) 2026 Will Cohen
//
// Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Turns a methods object + busy/destroy classification into a
// ModuleHandler-compatible async factory. Every method chains on a shared
// workerQueue, so calls run fully serially per handler: `workerQueue =
// next.catch(() => {})` makes the next call await the prior body while the
// caller still sees the real (possibly rejected) `next`. A destroy chained
// behind an in-flight ccall cannot run until that ccall has torn down.
//
// Because the queue is fully serial, `inFlight` never exceeds 1 and the
// destroy gate's awaitDrain() is a no-op today. The busy/destroy
// classification is kept for a future queue that serializes entry only.
//
// The diagnostic substrate is opt-in and off by default. When it is off, a
// call pays one truthiness check. setLogConfig({level, categories}) turns it
// on. level is one of off, error, warn, info, debug or trace. A category is
// the lowercase part of the tag before the first '-', so 'BUSY-INC' gives
// 'busy'. Events go to a ring of 256 entries, which flushes to stderr on an
// uncaught error or an unhandled rejection.
//
// A worker receives the settings through initArgs.handlerRuntime, which holds
// {logLevel, logCategories}. __setWorkerSlot(N) puts the pool-slot index onto
// the events.

const LEVEL_RANK = { off: 0, error: 1, warn: 2, info: 3, debug: 4, trace: 5 };
const VALID_LEVELS = Object.keys(LEVEL_RANK);

// Module-level config. dbg() and flushDebugRing() read this. They do not read
// the environment or globalThis. `level: null` means off. `categories: null`
// lets every category pass. If categories is a list, it is the filter.
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
  // Both fields are validated before either is written. Writing as we go
  // leaves a bad `categories` call with a new `level` already committed, so
  // the throw would report failure while having changed the log state.
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

// Category derivation: lowercased prefix of `tag` up to the first '-'.
// 'BUSY-INC' -> 'busy', 'EXPLICIT-DISPOSE' -> 'explicit', 'FR-CALLBACK-SUPPRESSED' -> 'fr'.
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

// Use Date.now() (wall-clock ms) so events from worker_threads / web
// workers and the host process can be ordered against each other.
// performance.now() is monotonic-from-process-start with a different
// origin per worker, so ordering across processes via perf.now would
// be wrong.
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

// Internal unconditional emitter. Writes the line to the ring buffer
// and stdout without consulting logState. Use dbg() for level-gated
// callsites and dbgPaired() for callers who have already captured the
// enabled flag at the start of a paired event.
const dbgEmit = (tag, fields) => {
  const line = formatEvent(tag, fields);
  ringBuffer[ringIdx % RING_SIZE] = line;
  ringIdx++;
  console.log(line);
};

// `level` defaults to 'debug'. Callers that want a different severity
// pass it explicitly. All current callsites are debug-grade.
export const dbg = (tag, fields, level = 'debug') => {
  if (!isEventEnabled(tag, level)) return;
  dbgEmit(tag, fields);
};

// Predicate exposed so consumers can snapshot the enabled state at the
// opening event of a paired (start/end) lifecycle and use that snapshot
// to gate the closing event. Closes the boot-window asymmetry where
// setLogConfig flips level from off to on between an opening and closing
// event of one call, leaving the trace with an unmatched closing event.
export const isEnabled = (tag, level = 'debug') => isEventEnabled(tag, level);

// This function does not consult the live level and category gate. It emits
// only if `enabled` is true. Take one isEnabled() snapshot at the opening
// event of a lifecycle pair and pass it here for the closing event. The
// opening and closing events then always emit together, even if setLogConfig
// changes during the call. The function calls dbgEmit, so an emitted event
// still reaches the ring buffer for flushDebugRing.
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

// A value carrying bytes: a typed array, an ArrayBuffer, a Node Buffer. A
// numeric byteLength identifies one, and a plain array has none, so a plain
// array keeps its contents in a fingerprint and stays discriminating.
const byteCarrierLength = (v) =>
  (v !== null && typeof v === 'object' && typeof v.byteLength === 'number') ? v.byteLength : null;

// Only a plain object or an array is walked. Anything else with its own JSON
// form -- a Date, a class instance -- passes through to JSON.stringify, which
// is what it did before.
const isWalkable = (v) => {
  if (v === null || typeof v !== 'object') return false;
  if (Array.isArray(v)) return true;
  const proto = Object.getPrototypeOf(v);
  return proto === Object.prototype || proto === null;
};

// Swap every byte carrier for a length token before stringify.
//
// This is a pre-walk and not a JSON.stringify replacer on purpose: a Node
// Buffer's own toJSON turns it into {type, data} BEFORE a replacer sees it, so
// a replacer serializes every byte of the exact payload this guards against.
//
// `depth` bounds the walk, so a cycle cannot spin.
const withoutBytes = (value, depth) => {
  // JSON.stringify throws on a BigInt, and a wasm init payload carries them
  // for 64-bit sizes and pointers. Tokenize it here so such a payload keeps a
  // discriminating fingerprint instead of falling into the throw below.
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

// Fingerprint over the whole init payload. Bytes contribute their length, so a
// multi-megabyte database costs one number.
//
// JSON.stringify alone is wrong for such a payload in two ways. A typed array
// serializes as an index-keyed object, so a real database costs tens of
// megabytes of text on every worker's init. An ArrayBuffer serializes as `{}`,
// which makes two DIFFERENT payloads compare equal and silently defeats the
// re-init guard in the factory below.
const defaultFingerprint = (args) => {
  try { return JSON.stringify(withoutBytes(args ?? null, FINGERPRINT_DEPTH) ?? null); }
  catch (e) {
    // The old fallback was String(args). It collapses every un-stringifiable
    // payload to "[object Object]", so two different payloads compare equal
    // and the re-init guard in the factory passes silently. That is the same
    // failure this function exists to prevent, so fail at init instead.
    throw new Error(
      'makeHandler: init args are not fingerprintable ('
      + (e && e.message ? e.message : String(e))
      + '). Pass an explicit `fingerprint`, for example byteLengthFingerprint([...]).',
      { cause: e },
    );
  }
};

// Build a fingerprint function over named init-arg fields, applying the same
// bytes rule as defaultFingerprint. Name the fields when the handler must
// IGNORE an init arg that changes without a change to the loaded module. Take
// defaultFingerprint when every field counts.
//
// A named field tolerates a plain array as bytes too (its `length` stands in
// for a byte length), because naming it declares what it holds.
//
// `prefix` labels the handler in the re-init error message, which is otherwise
// two anonymous field lists.
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

// A wasm trap, which is a RuntimeError, and an emscripten `Aborted(...)`
// error do not cross the comlink RPC boundary cleanly. structuredClone of the
// original error can fail, or the `await` of the caller can stall. Convert the
// known shapes to a plain Error with the tag `wasmTrap: true`. The rejection
// then survives RPC, and a caller can branch on `.kind`.
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
  // Built-in emscripten heap inspector. When the consumer's init function
  // calls ctx.attachEmscriptenModule(module), the substrate auto-merges
  // {heap-bytes, brk} into every BUSY-INC / BUSY-DEC event for this
  // handler. A consumer that loads something other than emscripten omits the
  // call and gets no heap fields. The substrate owns the event tags and the
  // introspection logic. The consumer supplies only the Module pointer.
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
  // Substrate context handed to the consumer's init function as its
  // second argument. Stable for the lifetime of this makeHandler closure
  // (one per handler per worker), so consumers can stash a reference if
  // they need to call attachEmscriptenModule asynchronously inside init.
  //
  // `dbg` / `isEnabled` / `dbgPaired` are exposed so consumer modules
  // can emit substrate events without `import`ing handler-runtime
  // themselves. The import path matters: when a handler.mjs is
  // esbuild-bundled with handler_runtime inlined and its handler-
  // overrides.mjs is kept external as a sibling-relative import, a
  // bare `import { dbg } from 'ffi-wasm/handler-runtime'` inside the
  // overrides resolves to a DIFFERENT module instance than the bundled
  // inline, and the two `logState` copies drift. setLogConfig mutates
  // one, dbg in the override reads the other, and emissions are
  // silently suppressed. Pulling dbg from ctx forces consumer emissions
  // to share state with the bundled setLogConfig path.
  const substrateCtx = {
    attachEmscriptenModule(m) { emscriptenModule = m; },
    dbg,
    isEnabled,
    dbgPaired,
    // getLogConfig is here for a diagnostic consumer that must read the
    // current logState directly. It mirrors the module-level export and obeys
    // the same module-state rules. It returns the live view, not a snapshot.
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
  // wrap() tests destroy before busy, so a name in both sets takes the
  // destroy path and loses its busy accounting with no diagnostic. Reject
  // the overlap rather than resolve it silently.
  for (const name of busy) {
    if (destroy.has(name)) {
      throw new Error(`makeHandler: method '${name}' is in both busyMethods and destroyMethods`);
    }
  }

  let workerQueue = Promise.resolve();
  let inFlight = 0;
  // Monotonic per-handler counter. Threaded as `id` onto BUSY-INC,
  // BUSY-DEC, DESTROY-FIRE, DESTROY-COMPLETE. Lets trace consumers pair
  // start/end events across long captures without relying on ordinal
  // position or label matching alone.
  let callIdCounter = 0;
  // Pool-slot index for this handler instance. null until the pool
  // manager calls the reserved __setWorkerSlot method (see factory
  // below). A value that is not null goes onto BUSY and DESTROY events as
  // `slot=N`. formatEvent skips a null value, so a handler that runs outside a
  // pool, such as a host-side or single-worker test, emits no slot field.
  let runtimeSlot = null;
  // Pending barriers: each promise resolves the next time inFlight hits 0.
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
          // Snapshot once at FIRE so COMPLETE is guaranteed to pair with
          // it. Everything the events consume (the id tag, the field
          // object) is built only when enabled, so a disabled substrate
          // (the steady-state default) pays nothing per call beyond the
          // one truthiness check.
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
          // Snapshot once at INC so DEC is guaranteed to pair even if
          // setLogConfig raises the level mid-call. The field object and
          // its `inspectHeap()` (which calls the module's `_sbrk(0)`, a
          // wasm boundary crossing) are built only when enabled. With
          // the substrate off, a busy ccall does no heap probe and
          // allocates no event object.
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
    // Worker debug propagation: a consumer can pass
    //   { handlerRuntime: { logLevel, logCategories } }
    // through init args. The factory invokes setLogConfig on the worker
    // side before any wrap fires. On a page or on the host, the consumer
    // calls setLogConfig directly. This branch covers the worker case, where
    // the runtime of the worker is a separate JS context and a call inside the
    // process cannot reach it.
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
          // Reserved substrate method. Pool-wide broadcast (init-pool!'s
          // :handler-runtime opt) invokes this on each worker to mutate
          // module-level logState without going through any library's
          // wrap(), so it does not serialize behind in-flight calls.
          // The name collides only with a consumer-method called
          // __setLogConfig, which by convention is reserved.
          wrapped.__setLogConfig = (cfg) => setLogConfig(cfg);
          // Reserved substrate method. Pool managers (pool.cljc init-pool!)
          // call this once per worker after creation to thread the worker's
          // pool-slot index into the handler closure. Subsequent BUSY/DESTROY
          // events carry `slot=N`. Independent of substrate enablement: even
          // when logging is off, the slot is captured so a later setLogConfig
          // flip immediately emits events with full per-worker context.
          wrapped.__setWorkerSlot = (s) => { runtimeSlot = s; };
          cachedHandler = wrapped;
          return wrapped;
        } catch (e) {
          // Init failure rolls state back so the caller can retry with
          // corrected args. Without this rollback, the cached fingerprint
          // and rejected initPromise would refuse every subsequent call.
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
