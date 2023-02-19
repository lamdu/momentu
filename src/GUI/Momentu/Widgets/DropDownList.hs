-- | A vertical-expand (combo-like) choice widget
{-# LANGUAGE TemplateHaskell, DerivingVia, NamedFieldPuns #-}
module GUI.Momentu.Widgets.DropDownList
    ( make
    , defaultFdConfig
    , Config(..), defaultConfig
    , Orientation(..)
    , Texts(..), chooseSelected
    , englishTexts
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Property (Property(..))
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Direction (Orientation(..), perpendicular, axis)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.ModKey (noMods)
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.ListBox as ListBox

import           GUI.Momentu.Prelude

newtype Texts a = Texts
    { _chooseSelected :: a
    } deriving stock Eq
Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts

englishTexts :: Texts Text
englishTexts = Texts
    { _chooseSelected = "Choose selected"
    }

defaultFdConfig ::
    (MonadReader env m, Has (Texts Text) env, Has (MomentuTexts.Texts Text) env) =>
    Text -> m FocusDelegator.Config
defaultFdConfig helpCategory =
    Lens.view id <&> \txt ->
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [noMods ModKey.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc [helpCategory, txt ^. has . MomentuTexts.choose]
    , FocusDelegator.focusParentKeys = [ModKey.Key'Enter, ModKey.Key'Escape] <&> noMods
    , FocusDelegator.focusParentDoc = E.Doc [helpCategory, txt ^. has . chooseSelected]
    }

data Config = Config
    { ddFDConfig :: FocusDelegator.Config
    , ddListBox :: ListBox.Config
    }

defaultConfig ::
    (MonadReader env m, Has (Texts Text) env, Has (MomentuTexts.Texts Text) env) =>
    Text -> m Config
defaultConfig helpCategory =
    defaultFdConfig helpCategory <&>
    \ddFDConfig ->
    Config
    { ddFDConfig
    , ddListBox = ListBox.defaultConfig
    }

make ::
    ( Eq childId, MonadReader env m, Applicative f, Functor t, Foldable t
    , State.HasCursor env, Has Hover.Style env, Element.HasElemIdPrefix env
    , Glue.HasTexts env
    ) =>
    Property f childId -> t (childId, TextWidget f) -> Config -> ElemId -> m (TextWidget f)
make (Property curChild choose) children config myId =
    do
        listbox <- ListBox.make choose children (ddListBox config)
        let perp :: Lens' (Vector2 a) a
            perp = axis (perpendicular (ListBox.lbOrientation (ddListBox config)))
        let maxDim = children <&> (^. _2 . Element.size . perp) & maximum
        let closed = children ^?! Lens.folded . Lens.filteredBy (_1 . Lens.only curChild) . _2
        let anyChildFocused = Lens.has (Lens.folded . _2 . Align.tValue . Lens.filtered Widget.isFocused) children
        widget <-
            if anyChildFocused
            then
                do
                    c <- closed ^. Align.tValue & Hover.anchor
                    Hover.anchor (listbox ^. Align.tValue) >>= Hover.hover
                        <&> \h -> listbox & Align.tValue .~ [h] `Hover.hoverInPlaceOf` c
            else pure closed
        widget
            & Align.tValue (FocusDelegator.make (ddFDConfig config) FocusDelegator.FocusEntryParent myId)
            >>= Element.padToSize (0 & perp .~ maxDim) 0
