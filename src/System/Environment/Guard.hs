{-# LANGUAGE ImportQualifiedPost #-}

-- | Functions for conditionally running 'IO' actions based on an environment
-- variable.
--
-- @since 0.1
module System.Environment.Guard
  ( -- * High level combinators
    ExpectEnv (..),
    withGuard,
    withGuard_,
    guardOrElse,
    guardOrElse',

    -- * Low level functions

    -- ** Checking environment variable is set
    guardSet,
    guardSet_,

    -- ** Checking environment variable match
    guardEquals,
    guardEquals_,

    -- ** Checking environment variable predicate
    guardPredicate,
    guardPredicate_,
  )
where

import System.Environment.Guard.Lifted (ExpectEnv)
import System.Environment.Guard.Lifted qualified as Lifted

-- $setup
-- >>> import Control.Monad (void)
-- >>> import Data.Functor (($>))
-- >>> import System.Environment (setEnv)
-- >>> import System.Environment.Guard.Lifted (ExpectEnv (..))

-- | Guards an action behind an environment variable according to
-- the given expectation.
--
-- ==== __Examples__
-- >>> setEnv "FOO" "bar"
-- >>> withGuard "FOO" (ExpectEnvEquals "baz") (putStrLn "succeeded")
-- Nothing
--
-- >>> withGuard "FOO" ExpectEnvSet (putStrLn "succeeded")
-- succeeded
-- Just ()
--
-- @since 0.1.1
withGuard :: String -> ExpectEnv -> IO a -> IO (Maybe a)
withGuard = Lifted.withGuard

-- | Variant of 'withGuard' that ignores the return value.
--
-- @since 0.1.1
withGuard_ :: String -> ExpectEnv -> IO a -> IO ()
withGuard_ = Lifted.withGuard_

-- | @guardOrElse var expect io1 io2@ is equivalent to
-- @withGuard var expect io1@ except that it runs @io2@ if @io1@ is not run.
--
-- ==== __Examples__
-- >>> setEnv "FOO" "bar"
-- >>> guardOrElse "FOO" ExpectEnvSet (pure True) (pure "not found")
-- Right True
--
-- >>> guardOrElse "BAR" ExpectEnvSet (pure True) (pure "not found")
-- Left "not found"
--
-- @since 0.1.1
guardOrElse ::
  -- | The environment variable.
  String ->
  -- | The expectation.
  ExpectEnv ->
  -- | The action to run if the expectation succeeds.
  IO a ->
  -- | The action to run if the expectation fails.
  IO e ->
  -- | The result.
  IO (Either e a)
guardOrElse = Lifted.guardOrElse

-- | 'guardOrElse' specialized to the same type so that we always return an
-- @a@. This can also be used to ignore the return value i.e.
--
-- @
-- guardOrElse' var expect (void io1) io2
-- @
--
-- ==== __Examples__
-- >>> setEnv "FOO" "bar"
-- >>> guardOrElse' "FOO" ExpectEnvSet (pure True) (pure False)
-- True
--
-- >>> guardOrElse' "BAR" ExpectEnvSet (pure True) (pure False)
-- False
--
-- >>> guardOrElse' "BAR" ExpectEnvSet (void $ pure True) (putStrLn "not found")
-- not found
--
-- @since 0.1.1
guardOrElse' ::
  -- | The environment variable.
  String ->
  -- | The expectation.
  ExpectEnv ->
  -- | The action to run if the expectation succeeds.
  IO a ->
  -- | The action to run if the expectation fails.
  IO a ->
  -- | The result.
  IO a
guardOrElse' = Lifted.guardOrElse'

-- | @'guardSet' var io@ runs @io@ iff
--
-- 1. The environment variable @var@ is set.
--
-- @
-- 'guardSet' var === 'guardPredicate' var ('const' 'True')
-- @
--
-- ==== __Examples__
--
-- >>> guardSet "NOT_SET" (putStrLn "ran io" $> True)
-- Nothing
--
-- >>> setEnv "SET" "foo"
-- >>> guardSet "SET" (putStrLn "ran io" $> True)
-- ran io
-- Just True
--
-- @since 0.1
guardSet :: String -> IO a -> IO (Maybe a)
guardSet = Lifted.guardSet

-- | Variant of 'guardSet' that ignores the return value.
--
-- @since 0.1
guardSet_ :: String -> IO a -> IO ()
guardSet_ = Lifted.guardSet_

-- | @'guardEquals' var expected io@ runs @io@ iff
--
-- 1. The environment variable @var@ is set.
-- 2. @var@'s value equals @expected@. This is __case-insensitive__.
--
-- @
-- 'guardEquals' var expected === 'guardPredicate' var (\\a b -> 'fmap' 'Data.Char.toLower' a == 'fmap' 'Data.Char.toLower' b)
-- @
--
-- ==== __Examples__
--
-- >>> guardEquals "NOT_SET" "val" (putStrLn "ran io" $> True)
-- Nothing
--
-- >>> setEnv "WRONG_VAL" "good_val"
-- >>> guardEquals "WRONG_VAL" "bad_val" (putStrLn "ran io" $> True)
-- Nothing
--
-- >>> setEnv "WILL_RUN" "val"
-- >>> guardEquals "WILL_RUN" "VAL" (putStrLn "ran io" $> True)
-- ran io
-- Just True
--
-- @since 0.2
guardEquals :: String -> String -> IO a -> IO (Maybe a)
guardEquals = Lifted.guardEquals

-- | Variant of 'guardEquals_' that ignores the return value.
--
-- @since 0.2
guardEquals_ :: String -> String -> IO a -> IO ()
guardEquals_ = Lifted.guardEquals_

-- | Variant of 'guardPredicate' that ignores the return value.
--
-- @since 0.1
guardPredicate_ :: String -> (String -> Bool) -> IO a -> IO ()
guardPredicate_ = Lifted.guardPredicate_

-- | This is the most general way to check an environment variable.
-- @'guardPredicate' var p io@ runs @io@ iff
--
-- 1. The environment variable @var@ is set.
-- 2. @var@'s value satisfies predicate @p@.
--
-- ==== __Examples__
--
-- >>> guardPredicate "NOT_SET" (const True) (putStrLn "ran io" $> True)
-- Nothing
--
-- >>> setEnv "CASE_WRONG" "VAL"
-- >>> guardPredicate "CASE_WRONG" (== "val") (putStrLn "ran io" $> True)
-- Nothing
--
-- >>> setEnv "WILL_RUN" "VAL"
-- >>> guardPredicate "WILL_RUN" (== "VAL") (putStrLn "ran io" $> True)
-- ran io
-- Just True
--
-- @since 0.1
guardPredicate :: String -> (String -> Bool) -> IO a -> IO (Maybe a)
guardPredicate = Lifted.guardPredicate
