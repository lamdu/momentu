{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Setup
    ( SetupOptions(..), setupWindowMode, setupLcdSubPixel, setupFontSize
        , defaultSetupOptions
    , OSString, defaultSetup, defaultMakeWidget, MakeWidget
    ) where

import qualified Control.Lens as Lens
import           Data.MRUMemo (memoIO)
import           GUI.Momentu.DefaultEnv (defaultEnv, DefaultEnvWithCursor, defaultEnvWithCursor)
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.Main as Main
import           GUI.Momentu.MetaKey (OSString)
import           GUI.Momentu.Widget (Widget(..))
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Window (MonitorInfo(..), WindowMode(..))
import qualified GUI.Momentu.Window as Window
import qualified GUI.Momentu.Zoom as Zoom
import           Graphics.UI.GLFW.Utils (withGLFW)
import qualified System.Info as SysInfo

import           GUI.Momentu.Prelude

data SetupOptions = SetupOptions
    { _setupWindowMode :: MonitorInfo -> WindowMode
    , _setupLcdSubPixel :: !Font.LCDSubPixelEnabled
    , _setupFontSize :: !Float
    }
Lens.makeLenses ''SetupOptions

defaultSetupOptions :: SetupOptions
defaultSetupOptions = SetupOptions
    { _setupWindowMode = Window.Windowed . (^. Window.monitorSizeOSLogicalPixels)
    , _setupLcdSubPixel = Font.LCDSubPixelEnabled
    , _setupFontSize = 24
    }

type MakeWidget = (Float -> IO Font) -> DefaultEnvWithCursor -> IO (Widget IO)

defaultSetup :: String -> FilePath -> SetupOptions -> MakeWidget -> IO ()
defaultSetup title fontPath options makeWidget =
    do
        win <- Window.create title (options ^. setupWindowMode)
        cachedOpenFont <- memoIO (Font.openFont (options ^. setupLcdSubPixel) ?? fontPath)
        let cachedOpenFontBySizeFactor = cachedOpenFont . (* options ^. setupFontSize)
        mainLoop <- Main.mainLoopWidget
        let os = SysInfo.os
        env <- cachedOpenFontBySizeFactor 1 <&> defaultEnv os
        opts <- Main.defaultOptions os env
        Main.run mainLoop win Main.Handlers
            { Main.makeWidget =
                defaultMakeWidget os cachedOpenFontBySizeFactor makeWidget
                <&> Lens.mapped %~ Widget.weakerEvents (Main.quitEventMap env)
            , Main.options = opts
            }
        & withGLFW

defaultMakeWidget :: OSString -> (Float -> IO Font) -> MakeWidget -> Main.Env -> IO (Widget IO)
defaultMakeWidget os getFont makeWidget mainLoopEnv =
    do
        sizeFactor <- Zoom.getZoomFactor (mainLoopEnv ^. Main.eZoom)
        font <- getFont sizeFactor
        makeWidget getFont (defaultEnvWithCursor os (mainLoopEnv ^. Main.eState) font)
