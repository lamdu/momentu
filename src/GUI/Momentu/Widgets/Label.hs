module GUI.Momentu.Widgets.Label
    ( make, makeFocusable
    ) where

import           GUI.Momentu.Align (WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (asElemId)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView

import           GUI.Momentu.Prelude

make ::
    ( MonadReader env m, Has TextView.Style env, Element.HasElemIdPrefix env
    , Has Dir.Layout env
    ) =>
    Text -> m (WithTextPos View)
make text = (TextView.make ?? text) <*> (Element.subElemId ?? asElemId text)

makeFocusable ::
    ( MonadReader env m, Applicative f, State.HasCursor env
    , Has Dir.Layout env
    , Has TextView.Style env, Element.HasElemIdPrefix env
    ) =>
    Text -> m (TextWidget f)
makeFocusable text =
    do
        toFocusable <- Widget.makeFocusableView
        widgetId <- Element.subElemId ?? asElemId text
        make text <&> Align.tValue %~ toFocusable widgetId
