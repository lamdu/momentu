-- | Responsive layout for expressions express the hierarchy using parentheses and indentation,
-- as is customary in many programming languages and in mathematics.

{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Responsive.Expression
    ( Style(..), indentBarWidth, indentBarGap, indentBarColor
    , disambiguators, mDisambiguators
    , boxSpacedMDisamb, indent
    , addParens
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader)
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId(..), asElemId)
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView

import           GUI.Momentu.Prelude

data Style = Style
    { _indentBarWidth :: Double
    , _indentBarGap :: Double
    , _indentBarColor :: Draw.Color
    } deriving (Eq, Show, Generic)
JsonTH.derivePrefixed "_" ''Style
Lens.makeLenses ''Style

disambiguators ::
    ( MonadReader env m, Functor f, Has Style env, Spacer.HasStdSpacing env
    , Has Dir.Layout env
    ) =>
    ElemId -> m (Options.Disambiguators f)
disambiguators i = Options.Disambiguators <$> pushToReader (addParens i) <*> pushToReader (indent i)

mDisambiguators ::
    ( MonadReader env m, Functor f, Has Style env, Spacer.HasStdSpacing env
    , Has Dir.Layout env
    ) =>
    Maybe ElemId -> m (Options.Disambiguators f)
mDisambiguators = maybe (pure Options.disambiguationNone) disambiguators

addParens ::
    ( MonadReader env m, Has TextView.Style env, Functor f, Has Dir.Layout env
    ) =>
    ElemId -> TextWidget f -> m (TextWidget f)
addParens i w =
    Lens.view id <&>
    \env ->
    let paren t = TextView.make t (i <> asElemId t) env
    in  paren "(" ||| w ||| paren ")"
    where
        Glue.Poly (|||) = Glue.mkPoly Glue.Horizontal Dir.LeftToRight

indent ::
    ( MonadReader env m, Functor f, Has Style env, Spacer.HasStdSpacing env
    , Has Dir.Layout env
    ) =>
    ElemId -> Responsive f -> m (Responsive f)
indent i r =
    do
        bWidth <- totalBarWidth
        env <- Lens.view id
        let f w = (indentBar i (w ^. Element.height) Glue./|/ pure w) env
        r
            & Responsive.rNarrow . Lens.argument . Responsive.layoutWidth -~ bWidth
            & Responsive.alignedWidget %~ f
            & pure

totalBarWidth :: (MonadReader env m, Has Style env, Spacer.HasStdSpacing env) => m Double
totalBarWidth =
    do
        s <- Lens.view has
        stdSpace <- Spacer.getSpaceSize <&> (^. _1)
        stdSpace * (s ^. indentBarWidth + s ^. indentBarGap) & pure

indentBar ::
    ( MonadReader env m, Has Style env, Spacer.HasStdSpacing env
    , Has Dir.Layout env
    ) =>
    ElemId -> Widget.R -> m View
indentBar myId height =
    do
        s <- Lens.view has
        stdSpace <- Spacer.getSpaceSize <&> (^. _1)
        let bar = Draw.backgroundColor (s ^. indentBarColor) (Spacer.make (Vector2 barWidth height)) bgElemId
            barWidth = stdSpace * s ^. indentBarWidth
            gapWidth = stdSpace * s ^. indentBarGap
        Glue.mkGlue Glue.Horizontal bar (Spacer.make (Vector2 gapWidth 0))
    where
        bgElemId = myId <> "("

boxSpacedMDisamb ::
    ( MonadReader env m, Applicative f, Has Style env, Spacer.HasStdSpacing env
    , Glue.HasTexts env
    ) =>
    Maybe ElemId -> [Responsive f] -> m (Responsive f)
boxSpacedMDisamb i r = mDisambiguators i >>= (`Options.boxSpaced` r)
