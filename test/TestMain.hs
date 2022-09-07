module Main (main) where

import qualified Tests.Hover
import qualified Tests.Momentu
import qualified Tests.WidgetGlue
import qualified Test.Tasty as Tasty

import           Prelude

main :: IO ()
main =
    Tasty.defaultMain tests
    where
        tests =
            Tasty.testGroup "Momentu Tests"
            [ Tests.Hover.test
            , Tests.Momentu.test
            , Tests.WidgetGlue.test
            ]
