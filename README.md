<div align="center">

# env-guard

[![Hackage](https://img.shields.io/hackage/v/env-guard)](https://hackage.haskell.org/package/env-guard)
[![MIT](https://img.shields.io/github/license/tbidne/env-guard?color=blue)](https://opensource.org/licenses/MIT)

[![nix](https://img.shields.io/github/workflow/status/tbidne/env-guard/nix/main?label=nix%209.2&&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/env-guard/actions/workflows/nix_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/env-guard/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/env-guard/actions/workflows/style_ci.yaml)

[![cabal 8.10](https://img.shields.io/github/workflow/status/tbidne/env-guard/cabal_8-10/main?label=8.10&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/env-guard/actions/workflows/cabal_8-10.yaml)
[![cabal 9.0](https://img.shields.io/github/workflow/status/tbidne/env-guard/cabal_9-0/main?label=9.0&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/env-guard/actions/workflows/cabal_9-0.yaml)
[![cabal 9.2](https://img.shields.io/github/workflow/status/tbidne/env-guard/cabal_9-2/main?label=9.2&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/env-guard/actions/workflows/cabal_9-2.yaml)

[![stack lts-18](https://img.shields.io/github/workflow/status/tbidne/env-guard/stack_lts-18/main?label=stack%20lts-18&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/env-guard/actions/workflows/stack_lts-18.yaml)
[![stack lts-19](https://img.shields.io/github/workflow/status/tbidne/env-guard/stack_lts-19/main?label=stack%20lts-19&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/env-guard/actions/workflows/stack_lts-19.yaml)
[![stack nightly](https://img.shields.io/github/workflow/status/tbidne/env-guard/stack_nightly/main?label=stack%20nightly&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/env-guard/actions/workflows/stack_nightly.yaml)

</div>

# Introduction

This package is used to guard `IO` actions behind an environment variable.

# Examples

## Simple

```haskell
guardSet :: String -> IO a -> IO (Maybe a)

-- Run function if env var AppEnv is set (to anything).
guardSet "AppEnv" runProd
```

```haskell
guardEquals :: String -> String -> IO a -> IO (Maybe a)

-- Run function if env var AppEnv is set to "prod" (case-insensitive).
guardEquals "AppEnv" "prod" runProd
```

```haskell
guardPredicate :: String -> (String -> Bool) -> IO a -> IO (Maybe a)

-- Run function if env var AppEnv's value satisfies the predicate.
guardPredicate "AppEnv" (\s -> s == "staging" || s == "dev") runTests
```

## Higher-Level Combinators

```haskell
-- withGuard uses the 'ExpectEnv' type to determine which of the above three
-- functions to call.
withGuard :: String -> ExpectEnv -> IO a -> IO (Maybe a)

-- equivalent to 'guardSet "BUILD_DOCS" buildDocs'.
withGuard "BUILD_DOCS" ExpectEnvSet buildDocs

-- equivalent to 'guardEquals "BUILD_DOCS" "true" buildDocs'.
withGuard "BUILD_DOCS" (ExpectEnvEquals "true") buildDocs
```

```haskell
-- guardOrElse runs one of two IO actions, depending on the success of the env
-- variable lookup.
guardOrElse :: String -> ExpectEnv -> IO a -> IO e -> IO (Either e a)

-- Runs runA if DO_A is set; otherwise runs runB.
guardOrElse "DO_A" ExpectEnvSet runA runB

-- guardOrElse' specialized to the same type.
guardOrElse' :: String -> ExpectEnv -> IO a -> IO a -> IO a

-- Runs runTests if RUN_TESTS is set; otherwise prints a message.
guardOrElse' "RUN_TESTS" ExpectEnvSet runTests (putStrLn "*** Tests Disabled ***")
```