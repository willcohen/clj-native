# clj-native

[![CI](https://github.com/willcohen/clj-native/actions/workflows/ci.yml/badge.svg)](https://github.com/willcohen/clj-native/actions/workflows/ci.yml)
[![Clojars](https://img.shields.io/clojars/v/net.willcohen/native.svg)](https://clojars.org/net.willcohen/native)
[![npm](https://img.shields.io/npm/v/ffi-wasm.svg)](https://www.npmjs.com/package/ffi-wasm)

clj-native has helper utilities for native libraries and FFI in Clojure and
Squint (ClojureScript). A library that uses clj-native binds a C or WASM API
one time. The library can then run on three backends.

## Install

npm: [`ffi-wasm`](https://www.npmjs.com/package/ffi-wasm). Clojars:
[`net.willcohen/native`](https://clojars.org/net.willcohen/native), with the
namespaces `net.willcohen.native.*`. The badges show the version. Do not use a
version range, because the API can change.

## Backends

Two properties select the backend. The first is the host, the JVM or
JavaScript. The second is the compiled artifact, a native shared library or an
emscripten `.wasm`.

|              | native `.so`/`.dylib`  | emscripten `.wasm`         |
|--------------|------------------------|----------------------------|
| **JVM host** | Panama, through dt-ffi | GraalWasm, through ccall   |
| **JS host**  | not possible           | worker pool, through ccall |

Because a JavaScript host cannot load a native shared library, the fourth cell
has no backend. The JS worker pool runs on Node `worker_threads` or in a
browser.

The two WASM backends call C functions through the emscripten `ccall`, because
they use the same artifact. The FFI backend binds each symbol directly. It does
not use a type list at call time.

On the JVM, `try-init!` in `platform-state` runs the FFI bootstrap and records
`:ffi`. If the bootstrap throws, `try-init!` runs the GraalVM bootstrap and
records `:graal`. The GraalVM backend has a different memory model and a
polyglot lock. The namespace docstring of `graal-wasm` gives the lock rules.
The docstring of `bootstrap-graal-module!` gives the loader contract.

### The build layer

The build layer makes the artifacts that the three backends load. It has
`net.willcohen.native.build` (in `src/bb`), `gen-handler` and `flake.nix`. It
makes these artifacts:

- the native shared library for the FFI backend
- the emscripten `.wasm` and its loader, for the two WASM backends
- the handler module for the JS worker pool, with `gen-handler`

The namespace docstring of `graal-wasm` gives the exports that the emscripten
module must have. The `build.clj` at the repository root is a different file.
It is the tools.build script for the jar of clj-native.

## Two hosts, one artifact

Because the two WASM backends read the same heap through different hosts, each
capability has one implementation for each host.

| Capability | JVM (`graal-wasm`) | JS (npm) |
|---|---|---|
| ccall | `ccall` | `handler-heap/ccallMethod` |
| heap read and write | `read-heap-array`, `heap-write-bytes!` | `heapHelpers` `heap*_get`, `heap*_set` |
| malloc, free, UTF-8 | `malloc`, `free-on-heap`, `utf8->string` | `heapHelpers` `malloc`, `free`, `utf8_to_string` |
| synchronous host HTTP | `net.willcohen.native.http` | `http-bridge` plus `fetch-worker` |
| host callback into C | `callbacks` (Panama upcall), `graal-wasm/put-js-globals!` | a consumer handler method |
| string-array walk | `string-array-pointer->strs` | `heapHelpers` `read_string_array` |
| struct read | `read-struct` | none. The consumer writes it. |

Each `heapHelpers` entry is a key of the object that
`heapHelpers(getModule)` returns. Spread that object into the methods map of
the consumer, before the keys of the consumer. A subsequent key with the same
name replaces the entry.

The JS side has no struct read. A JS consumer writes the walk in its own
overrides module, where the field layout of its library already is. On the
JVM, `read-struct` takes the layout as an argument.

## Contents

The npm package [`ffi-wasm`](https://www.npmjs.com/package/ffi-wasm) contains
the hand-written `.mjs` runtime helpers and the squint-compiled `.cljc`
modules. The `exports` map in `package.json` is the list. The package also
contains `macros.cljc`, because squint expands the macros at compile time.
Only the jar contains the other `.cljc` files.

The jar [`net.willcohen/native`](https://clojars.org/net.willcohen/native)
contains these thirteen namespaces. The docstring of each namespace is its
reference.

| Namespace | What it holds |
|---|---|
| `dispatch` | The per-function call engine, and the entry point |
| `macros` | Surface-generation helpers, for a consumer macro |
| `workload-pool` | The registry above `pool`, and the JVM executor slots |
| `pool` | The worker-router wrapper, on CLJS |
| `platform-state` | Impl selection, and the init flow |
| `platform` | JVM platform detection, and the FFI bootstrap |
| `graal-wasm` | The JVM WASM backend, on GraalVM polyglot |
| `ffi-mem` | JVM native-memory primitives, for the Panama backend |
| `callbacks` | JVM dt-ffi upcall registration |
| `http` | The host HTTP transport that a wasm or native library calls |
| `gen-handler` | The build-time handler-module generator |
| `build` | The consumer-facing build primitives, in `src/bb` |
| `test-runner` | The `cljs.test` runner footer |

To use clj-native, make one library value with `dispatch/library`. Then use
`dispatch/call!` for each C function. Register one handler spec for each
library with `workload-pool/register-handler!`.

## Build, test and deploy

```sh
bb build:js          # compile the npm .cljc modules to .mjs
bb jar               # build the JVM jar (net.willcohen/native)
bb test              # run test:clj, test:cljs and test:bb
bb deploy:npm        # publish ffi-wasm to npm
bb deploy:clojars    # publish net.willcohen/native to Clojars
```

The test directories divide the suites by runtime:

- `test/clj/`: JVM suites (`clojure.test`), for `bb test:clj`.
- `test/cljc/`: suites for the two runtimes. `test:clj` and `test:cljs` run
  the same body.
- `test/cljs/`: CLJS suites (`cljs.test`), for `bb test:cljs`. squint compiles
  them, and Node runs them.
- `test/bb/`: tests of the babashka build helpers, for `bb test:bb`.

`bb test:cljs` runs the pool suite with `node --expose-gc`. The suite starts a
garbage collection to do a test of its `FinalizationRegistry` sweeps. Without
the flag, the suite fails.

## Nix flake

Add the flake as an input to a build or a dev shell:

```nix
inputs.clj-native.url = "github:willcohen/clj-native";
```

`devShells.default` is the dev shell of clj-native. It has the Clojure
toolchain and Node for the tests.

`lib.<system>.mkCrossShells` makes a dev shell for a library that binds a C
dependency. `lib.<system>` also has `actualSystem`.

### Linux and Windows libs with zig

The dev shell has zig, `readelf` and `llvm-readobj`. zig builds the Linux
and Windows libs on a Linux or a macOS host.
`net.willcohen.native.build/zig-toolchain!` writes the compiler wrappers for
one of five resource dirs:

| Dir | zig target | Loads on |
|---|---|---|
| `linux-amd64`, `linux-aarch64` | `<arch>-linux-gnu.2.28` | glibc 2.28 and later |
| `linux-amd64-musl`, `linux-aarch64-musl` | `<arch>-linux-musl` | musl |
| `windows-amd64` | `x86_64-windows-gnu` | Windows 10 and later |

zig links libc++, libc++abi, libunwind and compiler-rt into the lib, in
place of libstdc++ and libgcc. Their license is Apache-2.0 WITH
LLVM-exception. zig links libc dynamically. Of glibc, a `linux-<arch>` lib
holds only the libc_nonshared wrappers (such as stat and atexit), which
glibc's link exception covers. A `windows-amd64` lib holds mingw-w64 runtime
code, under the ZPL 2.1 and permissive licenses. Ship their notices with the
lib.

With the env and the CMake args of `zig-toolchain!`, a build finds no
library, header or pkg-config file on the build machine. Give each
dependency to CMake by its path, for example `-DZLIB_LIBRARY`.

After the build, call `net.willcohen.native.build/check-linux-lib!` on the
lib. It throws when the lib has a run path. It also throws when the lib loads a
library or a glibc version that is not permitted for its dir.

On Windows, call `check-windows-lib!` on the DLL. It throws when the DLL
imports a DLL other than KERNEL32, SHELL32 or the UCRT (`api-ms-win-crt-*`),
such as a mingw runtime DLL.

On macOS, call `check-darwin-lib!` on the dylib. It throws when the dylib
loads a library that is not a system library, or has a run path. A run path
names a dir of the build machine.

On musl Linux, `extract-and-bind-library!` tries `<os>-<arch>-musl/` first,
and then `<os>-<arch>/`.

## License

Apache-2.0 WITH LLVM-exception. Refer to [`LICENSE`](LICENSE).
