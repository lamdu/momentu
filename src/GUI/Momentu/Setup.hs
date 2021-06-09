{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Setup
    ( SetupOptions(..), setupWindowMode, setupLcdSubPixel, setupFontSize
        , defaultSetupOptions
    , defaultSetup
    ) where

import qualified Control.Lens as Lens
import           Data.MRUMemo (memoIO)
import           GUI.Momentu.DefaultEnv (defaultEnv, DefaultEnvWithCursor, defaultEnvWithCursor)
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.Main as Main
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Widget (Widget(..))
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Window (WindowMode(..))
import qualified GUI.Momentu.Window as Window
import qualified GUI.Momentu.Zoom as Zoom
import           Graphics.UI.GLFW.Utils (withGLFW)

import           GUI.Momentu.Prelude

data SetupOptions = SetupOptions
    { _setupWindowMode :: !WindowMode
    , _setupLcdSubPixel :: !Font.LCDSubPixelEnabled
    , _setupFontSize :: !Float
    }
Lens.makeLenses ''SetupOptions

defaultSetupOptions :: SetupOptions
defaultSetupOptions = SetupOptions
    { _setupWindowMode = Window.Maximized
    , _setupLcdSubPixel = Font.LCDSubPixelEnabled
    , _setupFontSize = 24
    }

type MakeWidget = ((Float -> IO Font) -> DefaultEnvWithCursor -> IO (Widget IO))

defaultSetup :: String -> FilePath -> SetupOptions -> MakeWidget -> IO ()
defaultSetup title fontPath options makeWidget =
    do
        win <- Window.create title (options ^. setupWindowMode)
        cachedOpenFont <- memoIO (Font.openFont (options ^. setupLcdSubPixel) ?? fontPath)
        let cachedOpenFontBySizeFactor = cachedOpenFont . (* options ^. setupFontSize)
        mainLoop <- Main.mainLoopWidget
        opts <- cachedOpenFontBySizeFactor 1 <&> defaultEnv >>= Main.defaultOptions
        Main.run mainLoop win Main.Handlers
            { Main.makeWidget = defaultMakeWidget cachedOpenFontBySizeFactor makeWidget
            , Main.options = opts
            }
        & withGLFW

defaultMakeWidget :: (Float -> IO Font) -> MakeWidget -> Main.Env -> IO (Widget IO)
defaultMakeWidget getFont makeWidget mainLoopEnv =
    do
        sizeFactor <- Zoom.getZoomFactor (mainLoopEnv ^. Main.eZoom)
        font <- getFont sizeFactor
        makeWidget getFont (defaultEnvWithCursor (mainLoopEnv ^. Main.eState) font)
            <&> Widget.weakerEvents (EventMap.keysEventMap quitKeys quitDoc (error "Quit"))
    where
        quitKeys = [MetaKey.cmd MetaKey.Key'Q]
        quitDoc = EventMap.Doc ["Quit"]
