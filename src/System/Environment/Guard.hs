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
import Data.Functor (($>))
import System.Environment (lookupEnv)

-- $setup
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
-- *** IO action guarded by NOT_SET not run ***
-- Nothing
--
-- >>> setEnv "SET" "foo"
-- >>> guardSet "SET" (putStrLn "ran io" $> True)
-- ran io
-- Just True
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
-- ==== __Examples__
--
-- >>> guardExpected "NOT_SET" "val" (putStrLn "ran io" $> True)
-- *** IO action guarded by NOT_SET not run ***
-- Nothing
--
-- >>> setEnv "WRONG_VAL" "good_val"
-- >>> guardExpected "WRONG_VAL" "bad_val" (putStrLn "ran io" $> True)
-- *** IO action guarded by WRONG_VAL not run ***
-- Nothing
--
-- >>> setEnv "WILL_RUN" "val"
-- >>> guardExpected "WILL_RUN" "VAL" (putStrLn "ran io" $> True)
-- ran io
-- Just True
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
-- ==== __Examples__
--
-- >>> guardPredicate "NOT_SET" (const True) (putStrLn "ran io" $> True)
-- *** IO action guarded by NOT_SET not run ***
-- Nothing
--
-- >>> setEnv "CASE_WRONG" "VAL"
-- >>> guardPredicate "CASE_WRONG" (== "val") (putStrLn "ran io" $> True)
-- *** IO action guarded by CASE_WRONG not run ***
-- Nothing
--
-- >>> setEnv "WILL_RUN" "VAL"
-- >>> guardPredicate "WILL_RUN" (== "VAL") (putStrLn "ran io" $> True)
-- ran io
-- Just True
--
-- @since 0.1
guardPredicate :: String -> (String -> Bool) -> IO a -> IO (Maybe a)
guardPredicate var p io =
  lookupEnv var
    >>= \case
      Just result | p result -> Just <$> io
      _ ->
        putStrLn ("*** IO action guarded by " <> var <> " not run ***")
          $> Nothing

eqCaseInsensitive :: String -> String -> Bool
eqCaseInsensitive a b = fmap toLower a == fmap toLower b
