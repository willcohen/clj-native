# Changelog

## [Unreleased]

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
- A handler init that throws fails its task. A shutdown and a start wait for
  each other.
- HTTP bridge: a late response no longer loses the next request, and a
  crashed fetch worker keeps the count of its users.
- A crashed Node worker and a drained test run exit 1.

## [0.0.1] - 2026-07-30

First release.
