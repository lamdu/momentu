-- | A convenience API module that re-exports the functionality in the Momentu library

module GUI.Momentu
    (
    -- | Shortcut Keys
      ModKey.ModifierKeys(..), ModKey(..), ModKey.Key(..)
    , ModKey.noMods, ModKey.shift, ModKey.ctrl, ModKey.alt, MetaKey.cmd

    -- | Events
    , EventMap.EventMap
    , EventMap.Doc(..)
    , Widget.weakerEvents

    -- | Animations
    , ElemId

    -- | Element class
    , Element(..), Element.tint, Element.width, Element.height, Element.padAround

    -- | Widget
    , Widget
    , Widget.isFocused
    , State.Update

    -- | Responsive Widget
    , Responsive

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
    , Element.HasElemIdPrefix(..)
    , State.HasCursor(..)
    , State.GUIState(..)
    , State.readWidgetState
    , Widget.HasWidget(..)
    , Spacer.HasStdSpacing(..)

    -- | Setup
    , GLFWUtils.getPrimaryMonitor
    , GLFWUtils.getVideoModeSize
    , GLFWUtils.withGLFW

    , module GUI.Momentu.Window

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
import           GUI.Momentu.Animation (ElemId)
import           GUI.Momentu.DefaultEnv (DefaultEnv, DefaultEnvWithCursor, defaultEnv, defaultEnvWithCursor)
import qualified GUI.Momentu.Draw as MDraw
import           GUI.Momentu.Element (Element(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as EventMap
import qualified GUI.Momentu.Font as Font
import           GUI.Momentu.Glue ((/-/), (/|/), Glued)
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Setup as Setup
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           GUI.Momentu.Window
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           GUI.Momentu.Prelude

type MainLoopEnv = MainLoop.Env

runMainLoop :: MainLoop.MainLoop handlers -> GLFW.Window -> handlers -> IO ()
runMainLoop = MainLoop.run

type MainLoopOptions = MainLoop.Options
