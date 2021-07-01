{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Widgets.StdKeys
    ( DirKeys(..), keysLeft, keysRight, keysUp, keysDown
    , stdDirKeys
    , dirKey
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Direction (Orientation(..), Order(..))
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.ModKey as ModKey

import           GUI.Momentu.Prelude

data DirKeys key = DirKeys
    { _keysLeft :: [key]
    , _keysRight :: [key]
    , _keysUp :: [key]
    , _keysDown :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable)
Lens.makeLenses ''DirKeys
JsonTH.derivePrefixed "_keys" ''DirKeys

dirKey :: Dir.Layout -> Orientation -> Order -> Lens.ALens' (DirKeys key) [key]
dirKey _ Vertical Backward = keysUp
dirKey _ Vertical Forward = keysDown
dirKey Dir.LeftToRight Horizontal Backward = keysLeft
dirKey Dir.LeftToRight Horizontal Forward = keysRight
dirKey Dir.RightToLeft Horizontal Backward = keysRight
dirKey Dir.RightToLeft Horizontal Forward = keysLeft

stdDirKeys :: DirKeys ModKey.Key
stdDirKeys = DirKeys
    { _keysLeft  = [ModKey.Key'Left]
    , _keysRight = [ModKey.Key'Right]
    , _keysUp    = [ModKey.Key'Up]
    , _keysDown  = [ModKey.Key'Down]
    }
