{-# LANGUAGE LambdaCase #-}

-- | Functions for conditionally running 'IO' actions based on an environment
-- variable.
--
-- @since 0.1
module System.Environment.Guard
  ( -- * Checking environment variable is set
    guardSet,
    guardSet_,

    -- * Checking environment variable match
    guardExpected,
    guardExpected_,

    -- * Checking environment variable predicate
    guardPredicate,
    guardPredicate_,
  )
where

import Control.Monad (void)
import Data.Char (toLower)
import System.Environment (lookupEnv)

-- | @'guardSet' var io@ runs @io@ iff
--
-- 1. The environment variable @var@ is set.
--
-- @
-- 'guardSet' var === 'guardPredicate' var ('const' 'True')
-- @
--
-- @since 0.1
guardSet :: String -> IO a -> IO (Maybe a)
guardSet var = guardPredicate var (const True)

-- | Variant of 'guardSet' that ignores the return value.
--
-- @since 0.1
guardSet_ :: String -> IO a -> IO ()
guardSet_ var = void . guardSet var

-- | @'guardExpected' var expected io@ runs @io@ iff
--
-- 1. The environment variable @var@ is set.
-- 2. @var@'s value equals @expected@. This is __case-insensitive__.
--
-- @
-- 'guardExpected' var expected === 'guardPredicate' var (\\a b -> 'fmap' 'toLower' a == 'fmap' 'toLower' b)
-- @
--
-- @since 0.1
guardExpected :: String -> String -> IO a -> IO (Maybe a)
guardExpected var expected = guardPredicate var (eqCaseInsensitive expected)

-- | Variant of 'guardExpected_' that ignores the return value.
--
-- @since 0.1
guardExpected_ :: String -> String -> IO a -> IO ()
guardExpected_ var expected = void . guardExpected var expected

-- | Variant of 'guardPredicate' that ignores the return value.
--
-- @since 0.1
guardPredicate_ :: String -> (String -> Bool) -> IO a -> IO ()
guardPredicate_ var p = void . guardPredicate var p

-- | This is the most general way to check an environment variable.
-- @'guardPredicate' var p io@ runs @io@ iff
--
-- 1. The environment variable @var@ is set.
-- 2. @var@'s value satisfies predicate @p@.
--
-- @since 0.1
guardPredicate :: String -> (String -> Bool) -> IO a -> IO (Maybe a)
guardPredicate var p io =
  lookupEnv var
    >>= \case
      Just result | p result -> Just <$> io
      _ -> pure Nothing

eqCaseInsensitive :: String -> String -> Bool
eqCaseInsensitive a b = fmap toLower a == fmap toLower b