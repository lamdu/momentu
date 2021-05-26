module GUI.Momentu.Window
    ( WindowMode(..)
    , create, GLFW.Window
    ) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           GUI.Momentu.Prelude

data WindowMode = FullScreen | Maximized

create :: String -> WindowMode -> IO GLFW.Window
create title mode =
    do
        monitor <- GLFWUtils.getPrimaryMonitor
        videoModeSize <- GLFWUtils.getVideoModeSize monitor
        let createWin = GLFWUtils.createWindow title
        case mode of
            FullScreen -> createWin (Just monitor) videoModeSize
            Maximized  -> createWin Nothing (videoModeSize - 1)

