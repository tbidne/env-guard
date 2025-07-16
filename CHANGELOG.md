# Revision history for env-guard

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.2.1] -- 2025-07-17
### Changed
* Fixed `Show ExpectEnv` instance constructor name.
* Explicitly Re-export `ExpectEnv` data constructors from
  `System.Environment.Guard`.
* Doctests use cabal external command, hence no longer part of test suite.
  Therefore `--enable-tests` will no longer incur a GHC dependency.

## [0.2] -- 2022-07-02
### Changed
* Rename `guardExpected` functions to `guardEquals` to better match ExpectEnv type.

## [0.1.1] -- 2022-07-01
### Added
* Add higher-level `withGuard` combinator.
* Add "OrElse" pattern.

## [0.1] -- 2022-06-29

* First version. Released on an unsuspecting world.

[0.2.1]: https://github.com/tbidne/env-guard/compare/0.2..0.2.1
[0.2]: https://github.com/tbidne/env-guard/compare/0.1.1..0.2
[0.1.1]: https://github.com/tbidne/env-guard/compare/0.1..0.1.1
[0.1]: https://github.com/tbidne/env-guard/releases/tag/0.1
