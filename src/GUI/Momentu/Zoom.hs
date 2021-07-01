{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module GUI.Momentu.Zoom
    ( Zoom, make, eventMap, getZoomFactor
    , Config(..), defaultConfig
    , MetaKey.OSString
    , makeUnscaled
    , Texts(..), zoom, englishTexts
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.IORef
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as Texts
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey)
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFW.Utils

import           GUI.Momentu.Prelude

data Texts a = Texts
    { _zoom :: a
    , _enlarge :: a
    , _shrink :: a
    } deriving Eq
Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts

englishTexts :: Texts Text
englishTexts = Texts
    { _zoom = "Zoom"
    , _enlarge = "Enlarge"
    , _shrink = "Shrink"
    }

data Config key = Config
    { _shrinkKeys :: [key]
    , _enlargeKeys :: [key]
    , _enlargeFactor :: Double
    , _shrinkFactor :: Double
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
JsonTH.derivePrefixed "_" ''Config

Lens.makeLenses ''Config

defaultConfig :: MetaKey.OSString -> Config ModKey
defaultConfig os =
    Config
    { _shrinkKeys = [MetaKey.cmd os ModKey.Key'Minus]
    , _enlargeKeys = [MetaKey.cmd os ModKey.Key'Equal]
    , _shrinkFactor = 1.1
    , _enlargeFactor = 1.1
    }

newtype Zoom = Zoom
    { _scaleFactorRef :: IORef Widget.R
    }

eventMap ::
    (Has (Texts Text) env, Has (Texts.Texts Text) env) =>
    Zoom -> env -> Config ModKey -> EventMap (IO State.Update)
eventMap (Zoom ref) env config =
    mconcat
    [ modifyIORef ref (* config ^. enlargeFactor)
        & E.keysEventMap (config ^. enlargeKeys)
        (E.toDoc env [has . Texts.view, has . zoom, has . enlarge])
    , modifyIORef ref (/ config ^. shrinkFactor)
        & E.keysEventMap (config ^. shrinkKeys)
        (E.toDoc env [has . Texts.view, has . zoom, has . shrink])
    ]

getZoomFactor :: Fractional a => Zoom -> IO a
getZoomFactor (Zoom ref) = readIORef ref <&> realToFrac

make :: GLFW.Window -> IO Zoom
make win =
    do
        displayScale <- GLFW.Utils.getDisplayScale win
        winSize <- GLFW.Utils.windowSize win
        let winSizeFactor = winSize ^. _2 / 1080 & max 1
        newIORef (displayScale ^. _2 * winSizeFactor) <&> Zoom

-- | Useful mainly for tests
makeUnscaled :: Widget.R -> IO Zoom
makeUnscaled = fmap Zoom . newIORef
