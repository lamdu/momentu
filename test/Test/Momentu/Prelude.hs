-- | A prelude for Momentu tests

module Test.Momentu.Prelude
    ( module X
    ) where

import           GUI.Momentu.Prelude as X
import           Test.Framework as X
import           Test.Framework.Providers.HUnit as X (testCase)
import           Test.Framework.Providers.QuickCheck2 as X (testProperty)
import           Test.HUnit as X (assertString, assertEqual, assertFailure, assertBool)
import           Test.Momentu.Instances ()
