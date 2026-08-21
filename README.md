# clj-native

[![CI](https://github.com/willcohen/clj-native/actions/workflows/ci.yml/badge.svg)](https://github.com/willcohen/clj-native/actions/workflows/ci.yml)
[![Clojars](https://img.shields.io/clojars/v/net.willcohen/native.svg)](https://clojars.org/net.willcohen/native)
[![npm](https://img.shields.io/npm/v/ffi-wasm.svg)](https://www.npmjs.com/package/ffi-wasm)

Helper utilities for native libraries and FFI in the Clojure and Squint
(ClojureScript) ecosystems.

A library binds a C or WASM API one time, and then runs on three backends. On
the JVM it uses FFI over a native shared library, or GraalVM polyglot WASM. On
JavaScript it uses Node `worker_threads` or the browser.

## Install

npm: [`ffi-wasm`](https://www.npmjs.com/package/ffi-wasm). Clojars:
[`net.willcohen/native`](https://clojars.org/net.willcohen/native), namespaces
`net.willcohen.native.*`. The badges above show the current version. Pin the
exact version: the API still moves.

## Three backends, on two axes

Two axes decide the backend. The host is the JVM or JavaScript. The compiled
artifact is a native shared library or an emscripten `.wasm`. Three of the four
cells exist:

|              | native `.so`/`.dylib` | emscripten `.wasm`      |
|--------------|-----------------------|-------------------------|
| **JVM host** | Panama, through dt-ffi | GraalWasm, through ccall |
| **JS host**  | impossible            | worker pool, through ccall |

A JS host cannot bind a native shared library, so the fourth cell stays empty.

The two WASM backends use ccall because they share the artifact, and not the
host. The FFI backend has real symbols, so it binds them directly and needs no
type list at call time. This difference decides most of the design of this
library. The namespace docstring of `dispatch` gives the mechanism.

The GraalVM backend is not a fallback for FFI, although `try-init!` tries FFI
first. It is a second backend. It has its own memory model, heap helpers,
loader contract, and polyglot lock. The lock serializes one shared Context. A
pool worker can hold its own Context on a shared Engine instead. The namespace
docstring of `graal-wasm` gives the procedure.

### The build layer sits below all three

`net.willcohen.native.build`, `gen-handler`, `flake.nix` and the Containerfile
are not a fourth backend. They produce what the three backends consume:

- the native shared library that the FFI backend binds, through the four
  cross-compile shells
- the emscripten `.wasm` and its loader, which both WASM backends load. The
  namespace docstring of `graal-wasm` lists the exact module members that the
  emcc link must export
- the generated handler module that the JS worker pool loads, through
  `gen-handler`

The emcc half of the build layer serves only the two backends that share the
artifact.

Two files in this repository are named `build.clj`.
`src/bb/net/willcohen/native/build.clj` is the consumer-facing build API that
this section describes. The `build.clj` at the repository root is tools.build,
and it makes the jar of this project alone.

### Two hosts, one artifact: what exists twice

Each WASM backend reads the same heap through a different host. So each
capability has two implementations, one for each host. This is a consequence of
the axes above, and it is deliberate. A consumer author must know which
implementation they get.

| Capability | JVM (`graal-wasm`) | JS (npm) |
|---|---|---|
| ccall | `ccall` | `handler-heap/ccallMethod` |
| heap read and write | `read-heap-array`, `heap-write-bytes!` | `heapHelpers` `heap*_get`, `heap*_set` |
| malloc, free, UTF-8 | `malloc`, `free-on-heap`, `utf8->string` | `heapHelpers` `malloc`, `free`, `utf8_to_string` |
| synchronous host HTTP | `net.willcohen.native.http` | `http-bridge` plus `fetch-worker` |
| host callback into C | `callbacks` (Panama upcall), `graal-wasm/put-js-globals!` | a consumer handler method |
| string-array walk | `string-array-pointer->strs` | `heapHelpers` `read_string_array` |
| struct read | `read-struct` | none. The consumer writes it. |

`handler-heap` exports two names, `heapHelpers` and `ccallMethod`. Each
`heapHelpers` entry above is a key on the object that `heapHelpers(getModule)`
returns, and a consumer spreads that object into its methods map.

One row still costs a consumer work, by decision. The last two rows of the
table differ in whether the shape is library-specific.

A `char* const*` walk has no library-specific parameter at all. It is a
NUL-terminated table of pointer slots, and nothing about it varies by library.
So clj-native ships it on every host. A struct read is the opposite. The field
list, the type of each field and the byte offset of each field all come from
the headers of the library. `read-struct` takes those as an argument on the
JVM. A JS consumer writes the walk into its own overrides module, where that
layout already lives. A consumer may also read a list of structs at a count
and a stride, which `read-struct` does not express.

A consumer that already defined its own `read_string_array` keeps it. Spread
`handler-heap` first in the methods map, and a later key of the same name wins.

Two separate reasons produce this table, and they are easy to confuse. The
first is a choice. The JVM ccalls the module directly, and it does not drive
the module through a squint-compiled clj-native inside GraalJS. The second is
forced. A loader module that a polyglot Context evaluates cannot import a bare
specifier, so it cannot reach the shipped `.mjs` helpers. The notes at
`graal-wasm/ccall` and `graal-wasm/bootstrap-graal-module!` give the two
arguments.

clj-native supplies the shared parts:

- a worker-pool wrapper
- a per-function dispatch engine
- per-context resource lifetimes
- hand-written JS runtime helpers
- an Emscripten build-command helper
- a handler-module code generator

## Contents

There are two delivery surfaces.

- npm ([`ffi-wasm`](https://www.npmjs.com/package/ffi-wasm)): the hand-written
  `.mjs` runtime helpers, and the squint-compiled `.cljc`. The `exports` map
  in `package.json` is the list.
  Each hand-written helper starts with a header comment that states what the
  module does and what it assumes. A compiled module has only the license
  header, because squint emits no Clojure comment. Its description is
  the namespace docstring of the `.cljc` it came from, which the next bullet
  indexes.
- Clojars ([`net.willcohen/native`](https://clojars.org/net.willcohen/native)):
  the thirteen namespaces in the table below. The docstring of each one is the
  description of record.

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
| `build` | The consumer-facing build primitives, under `src/bb` |
| `test-runner` | The `cljs.test` runner footer |

`dispatch` is the entry point. A consumer builds one library value with
`library`, then calls `call!` for each function. The library value carries the
hooks, and there is no registry. `call!` routes each call to one of the three
cells of the table in "Three backends, on two axes" above. Both WASM cells
require a module that Emscripten produced, which is a deliberate limit.

`workload-pool` is the top of the pool stack. A consumer registers one
handler spec for each library with `register-handler!`, and the platform then
decides what it creates. On CLJS that is one worker-router joint pool
that every registered library shares, so a cross-library pipeline stays on one
worker. On the JVM it is three `ExecutorService` slots, `:mixed`, `:io` and
`:compute`. Consumers reach the pool itself through `wiring-pool`.

`pool` wraps [worker-router](https://github.com/willcohen/worker-router). It
adds library and context tracking, per-context worker affinity, context handles
that a WeakRef watches, and LRU eviction behind a minimum-age gate.

`try-init!` in `platform-state` is available on the JVM only. It runs the
FFI bootstrap. On any Throwable it uses GraalVM instead. It then records either
`:ffi` or `:graal`. It does not select `:node` or `:browser`, because a CLJS
consumer sets those values itself.

`resolveAsset` in `handler-paths` is available on Node only. It throws in a
browser.

## Building, testing, deploying

```sh
bb build:js          # compile every shipped .cljc -> .mjs (JS build)
bb jar               # build the JVM jar (net.willcohen/native)
bb test              # test:clj (JVM clojure.test) + test:cljs (squint cljs.test)
bb deploy:npm        # publish ffi-wasm to npm
bb deploy:clojars    # publish net.willcohen/native to Clojars
```

The tests run in two lanes, `test:clj` and `test:cljs`, over one namespaced
tree. Three directories divide the tree by runtime:

- `test/clj/`: suites for the JVM only (`clojure.test`).
- `test/cljc/`: dual-runtime `#?(:clj/:cljs)` suites. One body runs in both
  lanes.
- `test/cljs/`: suites for CLJS only (`cljs.test`). squint compiles these, and
  Node runs them.

`bb test:cljs` runs the pool suite under `node --expose-gc`. Its
`FinalizationRegistry` sweeps then run against a real collection. Without the
flag, those sweeps never run.

The npm tarball ships the built `.mjs` and also `macros.cljc`, because squint
expands the macros at compile time. The other `.cljc` files are JVM source.
They are in the jar, and not in the npm package.

## Nix flake

The flake is an input for a build or a dev shell:

```nix
inputs.clj-native.url = "github:willcohen/clj-native";
```

The flake exposes `lib.<system>.mkCrossShells`, which builds a parameterized
dev shell. On a Linux host it also builds four cross-compile shells:
`linuxAmd64Cross`, `linuxAarch64Cross`, `windowsAmd64Cross` and
`windowsArm64Cross`. It also exposes `baseBuildInputs`,
`crossPkgs`, `buildPkgs` and `actualSystem`. `devShells.default` is the shell
for clj-native itself: the Clojure toolchain, and Node for the CLJS test lane.

`flake.nix` and `flake.lock` are also in the jar, under
`net/willcohen/native/`. `cross-compile-in-container` extracts the two files.
A container build can then vendor a flake input that is pinned to the same
version as the code, with no network and no checkout. The flake outputs are
pure nix and never refer to `self`, so the two files stand alone.

## License

Apache-2.0 WITH LLVM-exception. See [`LICENSE`](LICENSE).
