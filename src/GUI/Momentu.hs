{-# LANGUAGE PatternSynonyms #-}
-- | A convenience API module that re-exports the functionality in the Momentu library

module GUI.Momentu
    (
    -- | Shortcut Keys
      MetaKey.ModifierKeys(..), MetaKey.noMods, MetaKey.cmd, MetaKey.shift
    , MetaKey(..)
    , MetaKey.Key(..)

    -- | Events
    , EventMap.Doc(..)
    , Widget.weakerEvents

    -- | Animations
    , AnimId

    -- | Element class
    , Element(..), Element.tint, Element.width, Element.height, Element.padAround

    -- | Widget
    , Widget
    , Widget.isFocused
    , WidgetId, pattern WidgetId
    , State.Update

    -- | View
    , View
    , View.unitSquare

    -- | GUI Layout
    , (/-/), (/|/), Glued

    -- | Widget alignment
    , Align.Aligned(..), Align.alignmentRatio, Align.value
    , Align.WithTextPos(..), Align.TextWidget, Align.textTop, Align.tValue

    -- | Drawing
    , Font.Font, Font.openFont, Font.LCDSubPixelEnabled(..)
    , MDraw.Color(..)
    , MDraw.backgroundColor
    , MDraw.addInnerFrame

    -- | Environments
    , Element.HasAnimIdPrefix(..)
    , State.HasCursor(..)
    , State.GUIState(..)
    , State.readWidgetState
    , Widget.HasWidget(..)
    , Spacer.HasStdSpacing(..)

    -- | Setup
    , GLFWUtils.getPrimaryMonitor
    , GLFWUtils.getVideoModeSize
    , GLFWUtils.withGLFW

    , Window.WindowMode(..)
    , createWindow, Window.Window

    -- | Main loop
    , Zoom, Zoom.getZoomFactor
    , MainLoopEnv, MainLoop.eZoom, MainLoop.eState, MainLoop.eWindowSize, MainLoop.quitEventMap
    , MainLoop.mainLoopWidget, MainLoop.defaultOptions, MainLoopOptions
    , MainLoop.Handlers(..), runMainLoop

    , DefaultEnv, defaultEnv
    , DefaultEnvWithCursor, defaultEnvWithCursor

    , Setup.defaultSetup
    , Setup.SetupOptions(..), Setup.setupWindowMode, Setup.setupLcdSubPixel, Setup.setupFontSize
    , Setup.defaultSetupOptions

    -- | Basic types
    , Vector2(..)
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import           GUI.Momentu.DefaultEnv (DefaultEnv, DefaultEnvWithCursor, defaultEnv, defaultEnvWithCursor)
import qualified GUI.Momentu.Draw as MDraw
import           GUI.Momentu.Element (Element(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as EventMap
import qualified GUI.Momentu.Font as Font
import           GUI.Momentu.Glue ((/-/), (/|/), Glued)
import qualified GUI.Momentu.Main as MainLoop
import           GUI.Momentu.MetaKey (MetaKey(..))
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.Setup as Setup
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Window as Window
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           GUI.Momentu.Prelude

type MainLoopEnv = MainLoop.Env

createWindow :: String -> Window.WindowMode -> IO GLFW.Window
createWindow = Window.create

type WidgetId = Widget.Id
pattern WidgetId :: AnimId -> Widget.Id
pattern WidgetId animId = Widget.Id animId

runMainLoop :: MainLoop.MainLoop handlers -> GLFW.Window -> handlers -> IO ()
runMainLoop = MainLoop.run

type MainLoopOptions = MainLoop.Options
