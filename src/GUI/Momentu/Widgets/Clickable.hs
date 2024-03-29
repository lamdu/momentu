{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Widgets.Clickable
    ( Config(..), action, doc, keys
    , make
    , makeText
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Align (WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.FocusDirection as Dir
import           GUI.Momentu.ModKey (ModKey)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView

import           GUI.Momentu.Prelude

data Config f = Config
    { _action :: f ElemId
    , _doc :: E.Doc
    , _keys :: [ModKey]
    }

Lens.makeLenses ''Config

make ::
    (MonadReader env m, Applicative f, State.HasCursor env) =>
    ElemId -> Config f -> WithTextPos View -> m (TextWidget f)
make myId config view =
    Align.tValue (Widget.makeFocusableWidgetWith myId enter . Widget.fromView) view
    <&> Align.tValue %~ Widget.weakerEvents eventMap
    where
        enter (Dir.Point _) = config ^. action
        enter _ = pure myId
        eventMap =
            config ^. action
            <&> State.updateCursor
            & E.keyPresses (config ^. keys) (config ^. doc)

makeText ::
    ( MonadReader env m, Applicative f, State.HasCursor env
    , Has Dir.Layout env
    , Has TextView.Style env
    ) =>
    ElemId -> Config f -> Text -> m (TextWidget f)
makeText myId config text = TextView.make text myId >>= make myId config
