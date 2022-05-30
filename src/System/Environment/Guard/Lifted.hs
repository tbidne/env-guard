{-# LANGUAGE LambdaCase #-}

-- | "System.Environment.Guard" for 'MonadIO'.
--
-- @since 0.1
module System.Environment.Guard.Lifted
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
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (toLower)
import System.Environment (lookupEnv)

-- $setup
-- >>> import Data.Functor (($>))
-- >>> import System.Environment (setEnv)

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

-- | @'guardExpected' var expected io@ runs @io@ iff
--
-- 1. The environment variable @var@ is set.
-- 2. @var@'s value equals @expected@. This is __case-insensitive__.
--
-- @
-- 'guardExpected' var expected === 'guardPredicate' var (\\a b -> 'fmap' 'toLower' a == 'fmap' 'toLower' b)
-- @
--
-- ==== __Examples__
--
-- >>> guardExpected "NOT_SET" "val" (putStrLn "ran io" $> True)
-- Nothing
--
-- >>> setEnv "WRONG_VAL" "good_val"
-- >>> guardExpected "WRONG_VAL" "bad_val" (putStrLn "ran io" $> True)
-- Nothing
--
-- >>> setEnv "WILL_RUN" "val"
-- >>> guardExpected "WILL_RUN" "VAL" (putStrLn "ran io" $> True)
-- ran io
-- Just True
--
-- @since 0.1
guardExpected :: MonadIO m => String -> String -> m a -> m (Maybe a)
guardExpected var expected = guardPredicate var (eqCaseInsensitive expected)

-- | Variant of 'guardExpected_' that ignores the return value.
--
-- @since 0.1
guardExpected_ :: MonadIO m => String -> String -> m a -> m ()
guardExpected_ var expected = void . guardExpected var expected

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
