-- | A vertical-expand (combo-like) choice widget
--
-- TODO: Highlight selected option and enter should go directly to it

module GUI.Momentu.Widgets.ListBox
    ( make
    , Config(..), defaultConfig
    , Orientation(..)
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Direction (Orientation(..))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Widget as Widget

import           GUI.Momentu.Prelude

newtype Config = Config { lbOrientation :: Orientation }

defaultConfig :: Config
defaultConfig = Config Vertical

make ::
    (MonadReader env m, Applicative f, Functor t, Foldable t, Glue.HasTexts env) =>
    m ((childId -> f ()) -> t (childId, TextWidget f) -> Config -> TextWidget f)
make =
    Glue.box <&>
    \box choose children config ->
    let orientation = lbOrientation config
        prependEntryAction (item, w) =
            w & Align.tValue . Widget.wState . Widget._StateUnfocused .
                Widget.uMEnter . Lens._Just . Lens.mapped . Widget.enterResultEvent %~ (choose item *>)
    in
    children <&> prependEntryAction & box orientation
