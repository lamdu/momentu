{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Control.Lens.Operators
import           Data.MRUMemo (memoIO)
import qualified GUI.Momentu as M
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Prelude.Compat

main :: IO ()
main =
    do
        win <- M.createWindow "Hello World" M.Maximized
        fontPath <- getDefaultFontPath
        cachedOpenFont <- memoIO (M.openFont M.LCDSubPixelEnabled ?? fontPath)
        helpFont <- cachedOpenFont 72
        mainLoop <- M.mainLoopWidget
        opts <- M.defaultOptions (M.defaultEnv helpFont)
        M.runMainLoop mainLoop win (hello opts cachedOpenFont)
    & M.withGLFW

hello :: M.MainLoopOptions -> (Float -> IO M.Font) -> M.Handlers
hello opts getFont =
    M.Handlers
    { M.makeWidget = makeWidget getFont
    , M.options = opts
    }

makeWidget :: (Float -> IO M.Font) -> M.MainLoopEnv -> IO (M.Widget IO)
makeWidget getFont mainLoopEnv =
    do
        sizeFactor <- M.getZoomFactor (mainLoopEnv ^. M.eZoom)
        font <- getFont (sizeFactor * 20)
        let env = M.defaultEnv font
        TextView.make env "Hello World!" ["hello"]
            ^. M.tValue
            & Widget.fromView
            & Widget.setFocused
            & M.weakerEvents (M.quitEventMap env)
            & pure
