module Main (main) where

import qualified Tests.Hover
import qualified Tests.Momentu
import qualified Tests.WidgetGlue
import           Test.Framework (defaultMain)

import           Prelude

main :: IO ()
main =
    defaultMain tests
    where
        tests =
            [ Tests.Hover.test
            , Tests.Momentu.test
            , Tests.WidgetGlue.test
            ]
