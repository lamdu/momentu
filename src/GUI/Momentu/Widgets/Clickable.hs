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
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.FocusDirection as Dir
import           GUI.Momentu.MetaKey (MetaKey, toModKey)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView

import           GUI.Momentu.Prelude

data Config f = Config
    { _action :: f Widget.Id
    , _doc :: E.Doc
    , _keys :: [MetaKey]
    }

Lens.makeLenses ''Config

make ::
    (MonadReader env m, Applicative f, State.HasCursor env) =>
    m (Widget.Id -> Config f -> WithTextPos View -> TextWidget f)
make =
    Widget.makeFocusableWidgetWith
    <&> \mkFocusableWith myId config view ->
    let enter (Dir.Point _) = config ^. action
        enter _ = pure myId
        eventMap =
            config ^. action
            <&> State.updateCursor
            & E.keyPresses (config ^. keys <&> toModKey) (config ^. doc)
        toButton w =
            Widget.fromView w
            & mkFocusableWith myId enter
            & Widget.weakerEvents eventMap
    in  view & Align.tValue %~ toButton

makeText ::
    ( MonadReader env m, Applicative f, State.HasCursor env
    , Has Dir.Layout env
    , Has TextView.Style env
    ) =>
    Widget.Id -> Config f -> Text -> m (TextWidget f)
makeText myId config text =
    (make ?? myId ?? config) <*> (TextView.make ?? text ?? Widget.toAnimId myId)
