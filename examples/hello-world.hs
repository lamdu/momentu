{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Control.Lens.Operators
import qualified GUI.Momentu as M
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Prelude.Compat

main :: IO ()
main =
    do
        fontPath <- getDefaultFontPath
        M.defaultSetup "Hello World" fontPath M.defaultSetupOptions makeWidget

makeWidget :: (Float -> IO M.Font) -> M.DefaultEnvWithCursor -> IO (Widget IO)
makeWidget _getFont env =
    TextView.make env "Hello World!" "hello"
    ^. M.tValue
    & Widget.fromView
    & Widget.setFocused
    & M.weakerEvents (M.quitEventMap env)
    & pure
