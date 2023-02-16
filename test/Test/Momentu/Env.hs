{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module Test.Momentu.Env where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Draw (Color(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.ModKey (ModKey, noMods)
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.StdKeys as StdKeys
import qualified System.Info as SysInfo

import           GUI.Momentu.Prelude

data Env = Env
    { _eDirLayout :: Dir.Layout
    , _eDirTexts :: Dir.Texts Text
    , _eGlueTexts :: Glue.Texts Text
    , _eGridTexts :: Grid.Texts Text
    , _eGridKeys :: Grid.Keys ModKey
    , _eDirKeys :: StdKeys.DirKeys ModKey
    , _eMomentuTexts :: MomentuTexts.Texts Text
    , _eElemId :: Anim.ElemId
    , _eHoverStyle :: Hover.Style
    }

env :: Env
env =
    Env
    { _eDirLayout = Dir.LeftToRight
    , _eDirTexts =
        Dir.Texts
        { Dir._left = "left"
        , Dir._right = "right"
        , Dir._up = "up"
        , Dir._down = "down"
        }
    , _eGlueTexts = Glue.Texts { Glue._stroll = "stroll" }
    , _eGridTexts =
        Grid.Texts
        { Grid._moreLeft = "more left"
        , Grid._moreRight = "more right"
        , Grid._top = "top"
        , Grid._bottom = "bottom"
        , Grid._leftMost = "left-most"
        , Grid._rightMost = "right-most"
        }
    , _eGridKeys = Grid.stdKeys SysInfo.os
    , _eDirKeys = StdKeys.stdDirKeys <&> noMods
    , _eMomentuTexts =
        MomentuTexts.Texts
        { MomentuTexts._edit = "Edit"
        , MomentuTexts._view = "View"
        , MomentuTexts._insert = "Insert"
        , MomentuTexts._delete = "Delete"
        , MomentuTexts._navigation = "Navigation"
        , MomentuTexts._move = "Move"
        , MomentuTexts._choose = "Choose"
        , MomentuTexts._forward = "Forward"
        , MomentuTexts._backward = "Backward"
        , MomentuTexts._language = "Language"
        }
    , _eElemId = "foo"
    , _eHoverStyle =
        Hover.Style
        { Hover._frameColor = Color 1 1 1 1
        , Hover._framePadding = pure 0
        , Hover._bgColor = Color 0 0 0 0
        , Hover._bgPadding = pure 0
        }
    }

Lens.makeLenses ''Env

instance Element.HasElemIdPrefix Env where animIdPrefix = eElemId
instance Has (Dir.Texts Text) Env where has = eDirTexts
instance Has (Glue.Texts Text) Env where has = eGlueTexts
instance Has (Grid.Keys ModKey) Env where has = eGridKeys
instance Has (Grid.Texts Text) Env where has = eGridTexts
instance Has (MomentuTexts.Texts Text) Env where has = eMomentuTexts
instance Has (StdKeys.DirKeys ModKey) Env where has = eDirKeys
instance Has Dir.Layout Env where has = eDirLayout
instance Has Hover.Style Env where has = eHoverStyle
