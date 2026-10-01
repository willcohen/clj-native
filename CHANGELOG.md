# Changelog

## [0.0.3] - 2026-09-30

### Added

- `:float32`, a C float, as an argument or a return type.
- `ffi-wasm/handler`, the worker side of ffi-wasm. `init-pool!` gives its
  URL to each handler as `ffiWasmHandlerUrl`.
- `:label` for `gen-handler-source`.

### Changed

- ffi-wasm ships its own modules in `dist/`: the page bundle
  `ffi-wasm.mjs`, and `handler.mjs` and the modules that the two share as
  files of their own, one copy of each. `./handler`, `./handler-*`,
  `./http-bridge`, `./fetch-worker` and `./test-runner` point at their own
  files, and each other subpath at `ffi-wasm.mjs`. Node and a bundler resolve
  the same subpaths as before. Of `src/`, the package ships only
  `macros.cljc` and `macros.mjs`.
- A page maps `ffi-wasm` to `dist/ffi-wasm.mjs`, and each subpath that it
  imports to the file of that subpath. It also maps `worker-router`,
  `worker-router/worker-bootstrap`, `comlink`, `resource-tracker` and
  `squint-cljs/`.
- Worker code imports `ffi-wasm/handler` or a worker-side subpath, not
  `ffi-wasm`. A bundler of a `dist/` file marks `node:*` as external.
- **Breaking.** `stage-test-deps!` takes `:native-dist`, the `dist/` dir of
  the installed ffi-wasm, in place of `:native-src`, and copies only
  `test_runner.mjs`.
- **Breaking.** A generated handler has no static import of ffi-wasm. It
  imports ffi-wasm from `ffiWasmHandlerUrl`. Its overrides module exports
  `methods(ffi)`, and `init` gets `ctx.ffi`. `:runtime-import-path` is gone.
- On Node, `createSyncFetch` defaults `workerUrl` to its own
  `fetch_worker.mjs`.

### Fixed

- The JVM writes the classes that dtype-next generates to a temp dir. A
  project gets no `./classes` dir.
- A zig `-O0` build links no UBSan runtime.

## [0.0.2] - 2026-09-28

### Added

- `:int64` returns, `:string?` args, and `with-library-context` for two
  libraries in one GraalVM process.
- `zig-toolchain!` builds Linux (glibc 2.28, musl) and Windows libs from any
  host. `check-linux-lib!`, `check-windows-lib!` and `check-darwin-lib!`
  check a lib before it ships.
- `build-once!`, and `:sha256` for `download-archive`.

### Changed

- Clojure 1.12.6, dtype-next 11.026, GraalVM 25.3.4.1, Node.js 22 or later.
- `null-ptr?` takes nil or 0 as NULL. GraalVM reads a NULL string as nil,
  and a ccall that throws makes `call!` throw, as on FFI.
- `make-native-fn-resolver` throws for a missing fn. Pool options are
  kebab-case only. `write-handler!` needs `:fingerprint-fields`, and
  `makeHandler` a `fingerprint`.

### Removed

- The cross shells and `cross-compile-in-container`. zig builds each lib.
- Unused options, arities and dead exports, `heapf64`, `getLogConfig` and
  `register-cmd-args!` among them.

### Fixed

- GraalVM starts from a jar and reads `/dev/urandom`.
- `extract-archive` leaves no partial dir when tar fails. It reads tar only.
- A host with no packaged native lib falls back to GraalVM.
- A handler init that throws fails its task, and a sync one can retry. A
  shutdown and a start wait for each other. `malloc` throws when it cannot
  allocate.
- HTTP bridge: a late response no longer loses the next request, and a
  crashed fetch worker keeps the count of its users.
- A crashed Node worker and a drained test run exit 1.

## [0.0.1] - 2026-07-30

First release.

[0.0.3]: https://github.com/willcohen/clj-native/compare/0.0.2...0.0.3
[0.0.2]: https://github.com/willcohen/clj-native/compare/0.0.1...0.0.2
[0.0.1]: https://github.com/willcohen/clj-native/releases/tag/0.0.1
