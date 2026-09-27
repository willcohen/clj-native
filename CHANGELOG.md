# Changelog

## [Unreleased]

### Added

- `:int64` returns, `:string?` args, and `with-library-context` for two
  libraries in one GraalVM process.

### Changed

- GraalVM reads a NULL string as nil.

### Fixed

- GraalVM starts from a jar and reads `/dev/urandom`.
- A crashed Node worker exits 1.

## [0.0.1] - 2026-07-30

First release.
