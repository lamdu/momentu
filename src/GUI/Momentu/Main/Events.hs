module GUI.Momentu.Main.Events
    ( Event(..), KeyEvent(..), MouseButtonEvent(..)
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.ModKey (Key, KeyState, ModifierKeys)
import qualified Graphics.UI.GLFW as GLFW

import           Prelude

data KeyEvent = KeyEvent
    { keKey :: Key
    , keScanCode :: Int
    , keState :: KeyState
    , keModKeys :: ModifierKeys
    } deriving (Show, Eq)

data MouseButtonEvent = MouseButtonEvent
    { mbButton :: GLFW.MouseButton
    , mbButtonState :: GLFW.MouseButtonState
    , mbModKeys :: ModifierKeys
    , mbPosition :: Vector2 Double
    -- ^ Position in frame buffer coordinates, which may not be the same as
    -- "window coordinates"
    , mbPositionInWindowCoords :: Vector2 Double
    -- ^ Position in "window coordinates".
    -- Since "retina" displays were introduced, window coordindates no longer
    -- relate to graphical pixels.
    } deriving (Show, Eq)

data Event
    = EventKey KeyEvent
    | EventChar Char
    | EventMouseButton MouseButtonEvent
    | EventWindowClose
    | EventWindowRefresh
    | EventDropPaths [FilePath]
    | EventFramebufferSize (Vector2 Int)
    deriving (Show, Eq)
