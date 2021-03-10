-- | A prelude for Momentu tests

module Test.Momentu.Prelude
    ( module X
    , runTest
    , assertSetEquals
    ) where

import qualified Control.Lens as Lens
import           Data.Set as Set
import           GUI.Momentu.Prelude as X
import           Test.Framework as X
import           Test.Framework.Providers.HUnit as X (testCase)
import           Test.Framework.Providers.QuickCheck2 as X (testProperty)
import           Test.HUnit as X (assertString, assertEqual, assertFailure, assertBool)
import           Test.Momentu.Instances ()

-- | Useful for running test in a terminal-incapable terminal, with
-- colors disabled
runTest :: Test -> IO ()
runTest test =
    defaultMainWithOpts [test]
    mempty { ropt_color_mode = Just ColorNever, ropt_threads = Just 1 }

assertSetEquals :: (Ord a, Show a) => String -> Set a -> Set a -> IO ()
assertSetEquals msg expected actual
    | expected == actual = pure ()
    | otherwise =
          msg
          <> asMsg "missing" missingElements
          <> asMsg "has unexpected" extraElements
          & fail
    where
        asMsg prefix s
            | Set.null s = ""
            | otherwise = " " <> prefix <> " " <> show (s ^.. Lens.folded)
        missingElements = expected `Set.difference` actual
        extraElements = actual `Set.difference` expected
