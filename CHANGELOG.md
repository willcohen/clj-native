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
- GraalVM reads a NULL string as nil.

### Removed

- The cross shells and `cross-compile-in-container`. zig builds each lib.

### Fixed

- GraalVM starts from a jar and reads `/dev/urandom`.
- `extract-archive` leaves no partial dir when tar fails. It reads tar only.
- A host with no packaged native lib falls back to GraalVM.
- A handler init that throws fails its task. A shutdown and a start wait for
  each other.
- A crashed Node worker exits 1.

## [0.0.1] - 2026-07-30

First release.
