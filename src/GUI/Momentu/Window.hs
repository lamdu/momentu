module GUI.Momentu.Window
    ( WindowMode(..)
    , create, GLFW.Window
    ) where

import           Data.Vector.Vector2 (Vector2)
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           GUI.Momentu.Prelude

data WindowMode = FullScreen | Windowed (Vector2 Int)

create :: String -> (Vector2 Int -> WindowMode) -> IO GLFW.Window
create title mode =
    do
        monitor <- GLFWUtils.getPrimaryMonitor
        videoModeSize <- GLFWUtils.getVideoModeSize monitor
        let createWin = GLFWUtils.createWindow title
        case mode videoModeSize of
            FullScreen -> createWin (Just monitor) videoModeSize
            Windowed size -> createWin Nothing (min <$> size <*> videoModeSize - 1)

