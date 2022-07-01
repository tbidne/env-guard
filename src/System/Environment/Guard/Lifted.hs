{-# LANGUAGE LambdaCase #-}

-- | "System.Environment.Guard" for 'MonadIO'.
--
-- @since 0.1
module System.Environment.Guard.Lifted
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

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (toLower)
import System.Environment (lookupEnv)

-- $setup
-- >>> import Data.Functor (($>))
-- >>> import System.Environment (setEnv)

-- | The expectation for an environment variable lookup.
--
-- @since 0.1.1
data ExpectEnv
  = -- | Expect that the environment variable is set
    -- (i.e. contents can be anything).
    --
    -- @since 0.1.1
    ExpectEnvSet
  | -- | Expect that the environment variable is set and the contents equals
    -- the string. This is __case-insensitive__.
    --
    -- @since 0.1.1
    ExpectEnvEquals String
  | -- | Expect that the environment variable is set and its contents
    -- satisfies the predicate.
    --
    -- @since 0.1.1
    ExpectEnvPredicate (String -> Bool)

-- | @since 0.1.1
instance Show ExpectEnv where
  showsPrec _ ExpectEnvSet = showString "ExpectEnvSet"
  showsPrec i (ExpectEnvEquals s) =
    showParen
      (i >= 11)
      (showString "ExpectEnvEquals " . showsPrec 11 s)
  showsPrec i (ExpectEnvPredicate _) =
    showParen
      (i >= 11)
      (showString "ExpectEnvEquals " . showString "_")

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
withGuard :: MonadIO m => String -> ExpectEnv -> m a -> m (Maybe a)
withGuard var expect m =
  case expect of
    ExpectEnvSet -> guardSet var m
    ExpectEnvEquals str -> guardEquals var str m
    ExpectEnvPredicate p -> guardPredicate var p m

-- | Variant of 'withGuard' that ignores the return value.
--
-- @since 0.1.1
withGuard_ :: MonadIO m => String -> ExpectEnv -> m a -> m ()
withGuard_ var expect = void . withGuard var expect

-- | @guardOrElse var expect m1 m2@ is equivalent to
-- @withGuard var expect m1@ except that it runs @m2@ if @m1@ is not run.
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
  MonadIO m =>
  -- | The environment variable.
  String ->
  -- | The expectation.
  ExpectEnv ->
  -- | The action to run if the expectation succeeds.
  m a ->
  -- | The action to run if the expectation fails.
  m e ->
  -- | The result.
  m (Either e a)
guardOrElse var expect m1 m2 =
  withGuard var expect m1
    >>= \case
      Just x -> pure $ Right x
      Nothing -> Left <$> m2

-- | 'guardOrElse' specialized to the same type so that we always return an
-- @a@. This can also be used to ignore the return value i.e.
--
-- @
-- guardOrElse' var expect (void m1) m2
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
  MonadIO m =>
  -- | The environment variable.
  String ->
  -- | The expectation.
  ExpectEnv ->
  -- | The action to run if the expectation succeeds.
  m a ->
  -- | The action to run if the expectation fails.
  m a ->
  -- | The result.
  m a
guardOrElse' var expect m = fmap (either id id) . guardOrElse var expect m

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
guardSet :: MonadIO m => String -> m a -> m (Maybe a)
guardSet var = guardPredicate var (const True)

-- | Variant of 'guardSet' that ignores the return value.
--
-- @since 0.1
guardSet_ :: MonadIO m => String -> m a -> m ()
guardSet_ var = void . guardSet var

-- | @'guardEquals' var expected io@ runs @io@ iff
--
-- 1. The environment variable @var@ is set.
-- 2. @var@'s value equals @expected@. This is __case-insensitive__.
--
-- @
-- 'guardEquals' var expected === 'guardPredicate' var (\\a b -> 'fmap' 'toLower' a == 'fmap' 'toLower' b)
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
guardEquals :: MonadIO m => String -> String -> m a -> m (Maybe a)
guardEquals var expected = guardPredicate var (eqCaseInsensitive expected)

-- | Variant of 'guardEquals_' that ignores the return value.
--
-- @since 0.2
guardEquals_ :: MonadIO m => String -> String -> m a -> m ()
guardEquals_ var expected = void . guardEquals var expected

-- | Variant of 'guardPredicate' that ignores the return value.
--
-- @since 0.1
guardPredicate_ :: MonadIO m => String -> (String -> Bool) -> m a -> m ()
guardPredicate_ var p = void . guardPredicate var p

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
guardPredicate :: MonadIO m => String -> (String -> Bool) -> m a -> m (Maybe a)
guardPredicate var p io =
  liftIO (lookupEnv var)
    >>= \case
      Just result | p result -> Just <$> io
      _ -> pure Nothing

eqCaseInsensitive :: String -> String -> Bool
eqCaseInsensitive a b = fmap toLower a == fmap toLower b
