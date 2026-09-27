# Changelog

## [Unreleased]

### Added

- `:int64` returns, `:string?` args, and `with-library-context` for two
  libraries in one GraalVM process.
- `build-once!`, and `:sha256` for `download-archive`.

### Changed

- GraalVM reads a NULL string as nil.

### Fixed

- GraalVM starts from a jar and reads `/dev/urandom`.
- `extract-archive` leaves no partial dir when tar fails. It reads tar only.
- A crashed Node worker exits 1.

## [0.0.1] - 2026-07-30

First release.
