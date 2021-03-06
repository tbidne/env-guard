module Main (main) where

import System.Environment (lookupEnv)
import Test.DocTest (doctest)
import Prelude

main :: IO ()
main = do
  shouldRun <- lookupEnv "RUN_DOCTEST"
  case shouldRun of
    Just _ -> doctest args
    _ -> putStrLn "*** Doctests Disabled ***"
  where
    args = files

files :: [String]
files =
  [ "-isrc",
    "src/System/Environment/Guard.hs",
    "src/System/Environment/Guard/Lifted.hs"
  ]
