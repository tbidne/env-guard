<div align="center">

# env-guard

[![Hackage](https://img.shields.io/hackage/v/env-guard)](https://hackage.haskell.org/package/env-guard)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/env-guard/ci.yaml?branch=main)](https://github.com/tbidne/env-guard/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/env-guard?color=blue)](https://opensource.org/licenses/MIT)

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