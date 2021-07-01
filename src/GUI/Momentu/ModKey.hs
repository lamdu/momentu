-- | ModKey type: Grouping the modifier keys with the key
{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.ModKey
    ( ModifierKeys(..), mControl, mAlt, mShift, mSuper
    , fromGLFWModifiers
    , ModKey(..), ctrlMods, altMods, shiftMods, superMods
    , GLFW.KeyState(..), GLFW.Key(..), GLFWUtils.charOfKey
    , noMods, ctrl, alt, shift, super
    , prettyKey
    , pretty
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.List (isPrefixOf)
import qualified Data.Text as Text
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import           Graphics.UI.GLFW.Instances ()
import qualified System.Info as SysInfo

import           GUI.Momentu.Prelude

data ModifierKeys = ModifierKeys
    { _mControl :: !Bool
    , _mAlt :: !Bool
    , _mShift :: !Bool
    , _mSuper :: !Bool
    }
    deriving stock (Generic, Show, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON)
Lens.makeLenses ''ModifierKeys

fromGLFWModifiers :: GLFW.ModifierKeys -> ModifierKeys
fromGLFWModifiers mods =
    ModifierKeys
    { _mControl = GLFW.modifierKeysControl mods
    , _mAlt = GLFW.modifierKeysAlt mods
    , _mShift = GLFW.modifierKeysShift mods
    , _mSuper = GLFW.modifierKeysSuper mods
    }

instance Semigroup ModifierKeys where
    ModifierKeys a0 b0 c0 d0 <> ModifierKeys a1 b1 c1 d1 =
        ModifierKeys (a0||a1) (b0||b1) (c0||c1) (d0||d1)
instance Monoid ModifierKeys where
    mempty = ModifierKeys False False False False

ctrlMods :: ModifierKeys
ctrlMods = mempty { _mControl = True }

altMods :: ModifierKeys
altMods = mempty { _mAlt = True }

shiftMods :: ModifierKeys
shiftMods = mempty { _mShift = True }

superMods :: ModifierKeys
superMods = mempty { _mSuper = True }

noMods :: GLFW.Key -> ModKey
noMods = ModKey mempty

ctrl :: GLFW.Key -> ModKey
ctrl = ModKey ctrlMods

alt :: GLFW.Key -> ModKey
alt = ModKey altMods

shift :: GLFW.Key -> ModKey
shift = ModKey shiftMods

super :: GLFW.Key -> ModKey
super = ModKey superMods

data ModKey = ModKey
    { _modifiers :: !ModifierKeys
    , _key :: GLFW.Key
    }
    deriving stock (Generic, Show, Eq, Ord)

prettyKey :: GLFW.Key -> Text
prettyKey k
    | "Key'" `isPrefixOf` show k = Text.pack $ drop 4 $ show k
    | otherwise = Text.pack $ show k

prettyModKeys :: ModifierKeys -> Text
prettyModKeys ms =
    mconcat $
    [superName | ms ^. mSuper] ++
    ["Ctrl+" | ms ^. mControl] ++
    ["Alt+" | ms ^. mAlt] ++
    ["Shift+" | ms ^. mShift]
    where
        superName
            | SysInfo.os /= "darwin" = "Win+"
            | ms == superMods = "⌘"
            | otherwise = "⌘+"

pretty :: ModKey -> Text
pretty (ModKey ms key) = prettyModKeys ms <> prettyKey key
