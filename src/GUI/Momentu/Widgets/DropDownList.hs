-- | A vertical-expand (combo-like) choice widget
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
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
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
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
    m (Text -> FocusDelegator.Config)
defaultFdConfig =
    Lens.view id <&> \txt helpCategory ->
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc [helpCategory, txt ^. has . MomentuTexts.choose]
    , FocusDelegator.focusParentKeys = [MetaKey.Key'Enter, MetaKey.Key'Escape] <&> MetaKey noMods
    , FocusDelegator.focusParentDoc = E.Doc [helpCategory, txt ^. has . chooseSelected]
    }

data Config = Config
    { ddFDConfig :: FocusDelegator.Config
    , ddListBox :: ListBox.Config
    }

defaultConfig ::
    (MonadReader env m, Has (Texts Text) env, Has (MomentuTexts.Texts Text) env) =>
    m (Text -> Config)
defaultConfig =
    defaultFdConfig <&> \defFd helpCategory ->
    Config
    { ddFDConfig = defFd helpCategory
    , ddListBox = ListBox.defaultConfig
    }

make ::
    ( Eq childId, MonadReader env m, Applicative f, Functor t, Foldable t
    , State.HasCursor env, Has Hover.Style env, Element.HasAnimIdPrefix env
    , Glue.HasTexts env
    ) =>
    m (Property f childId -> t (childId, TextWidget f) -> Config -> Widget.Id -> TextWidget f)
make =
    (,,,,)
    <$> Element.padToSize
    <*> ListBox.make
    <*> Hover.hover
    <*> Hover.anchor
    <*> FocusDelegator.make
    <&> \(padToSize, listbox, hover, anc, fd)
         (Property curChild choose) children config myId ->
    let perp :: Lens' (Vector2 a) a
        perp = axis (perpendicular (ListBox.lbOrientation (ddListBox config)))
        maxDim = children <&> (^. _2 . Element.size . perp) & maximum
        hoverAsClosed open = [hover (anc open)] `Hover.hoverInPlaceOf` anc (closed ^. Align.tValue)
        closed = children ^?! Lens.folded . Lens.filteredBy (_1 . Lens.only curChild) . _2
        anyChildFocused = Lens.has (Lens.folded . _2 . Align.tValue . Lens.filtered Widget.isFocused) children
        widget
            | anyChildFocused = listbox choose children (ddListBox config) & Align.tValue %~ hoverAsClosed
            | otherwise = closed
    in
    widget
    & Align.tValue %~ fd (ddFDConfig config) FocusDelegator.FocusEntryParent myId
    & padToSize (0 & perp .~ maxDim) 0
