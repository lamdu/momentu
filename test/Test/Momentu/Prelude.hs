-- | A prelude for Momentu tests

module Test.Momentu.Prelude
    ( module X
    ) where

import           GUI.Momentu.Prelude as X
import           Test.Tasty as X
import           Test.Tasty.HUnit as X (testCase)
import           Test.Tasty.QuickCheck as X (testProperty)
import           Test.HUnit as X (assertString, assertEqual, assertFailure, assertBool)
import           Test.Momentu.Instances ()
