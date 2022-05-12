{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Window
    ( WindowMode(..), _FullScreen, _Windowed
    , MonitorInfo(..), monitorSizeMillimeters, monitorSizeOSLogicalPixels
    , createWindow, GLFW.Window
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           GUI.Momentu.Prelude

data WindowMode = FullScreen | Windowed (Vector2 Int) -- ^ The OS-specific "logical pixel" size to use for window

-- NOTE: In macOS, the "logical pixels" are 2x2 physical pixels
--
-- In common Linux distributions with modern high density displays,
-- the "logical pixels" are the physical pixels
--
-- Do not attribute meaning to the OSLogicalPixels
data MonitorInfo = MonitorInfo
    { _monitorSizeMillimeters :: !(Vector2 Int)
    , _monitorSizeOSLogicalPixels :: !(Vector2 Int)
         -- ^ The OS-specific "logical pixel" size of the monitor
    }

createWindow :: String -> (MonitorInfo -> WindowMode) -> IO GLFW.Window
createWindow title mode =
    do
        monitor <- GLFWUtils.getPrimaryMonitor
        osLogicalPixels <- GLFWUtils.getVideoModeSize monitor
        millimeterSize <- GLFW.getMonitorPhysicalSize monitor <&> uncurry Vector2
        let createWin = GLFWUtils.createWindow title
        case mode (MonitorInfo millimeterSize osLogicalPixels) of
            FullScreen -> createWin (Just monitor) osLogicalPixels
            Windowed size -> createWin Nothing (min <$> size <*> osLogicalPixels - 1)

Lens.makeLenses ''MonitorInfo
Lens.makePrisms ''WindowMode
