{-# LANGUAGE TemplateHaskell #-}

module GUI.Momentu.Responsive.Options
    ( WideLayoutOption(..), wContexts, wLayout
    , wideNeedDisamib, wideUnambiguous
    , wideLayoutUnambiguousOption
    , tryWideLayout
    , hbox, table

    , Disambiguators(..), disambHoriz, disambVert
    , disambiguationNone
    , box, boxSpaced
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader)
import           Data.Functor.Compose (Compose(..))
import qualified Data.List as List
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive
    ( Responsive(..), WideLayouts(..), WideLayoutForm(..)
    , rWide, rNarrow, lWide, lWideDisambig, lForm
    , layoutWidth, vbox, fromView, vertLayoutMaybeDisambiguate
    )
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           GUI.Momentu.Prelude

data WideLayoutOption t f = WideLayoutOption
    { _wContexts ::
        Lens.AnIndexedTraversal WideLayoutForm
        (t (Responsive f)) (t (TextWidget f))
        (Responsive f) (WideLayouts f)
    , _wLayout :: t (TextWidget f) -> WideLayoutForm -> Maybe (WideLayouts f)
    }
Lens.makeLenses ''WideLayoutOption

tryWideLayout :: WideLayoutOption t a -> t (Responsive a) -> Responsive a -> Responsive a
tryWideLayout layoutOption elements fallback =
    case (layoutOption ^. wLayout) renderedElements form of
    Nothing -> fallback
    Just res ->
        Responsive
        { _rWide = res
        , _rNarrow =
            \layoutParams ->
            if wide ^. Align.tValue . Widget.wSize . _1 <= layoutParams ^. layoutWidth
            then wide
            else (fallback ^. rNarrow) layoutParams
        }
        where
            wide = res ^. lWide
    where
        (form, renderedElements) =
            elements & Lens.cloneIndexedTraversal (layoutOption ^. wContexts) %%@~ (\i x -> (i, x ^. rWide))

type HorizDisambiguator f = TextWidget f -> TextWidget f

makeWideLayouts :: HorizDisambiguator a -> TextWidget a -> WideLayoutForm -> WideLayouts a
makeWideLayouts disamb w form =
    WideLayouts
    { _lWide = w
    , _lWideDisambig = disamb w
    , _lForm = form
    }

wideUnambiguous :: Lens.AnIndexedTraversal WideLayoutForm (Responsive f) (TextWidget f) (Responsive f) (WideLayouts f)
wideUnambiguous = Lens.reindexed (^. rWide . lForm) Lens.selfIndex . Lens.iso id (^. lWide)

wideNeedDisamib :: Lens.AnIndexedTraversal WideLayoutForm (Responsive f) (TextWidget f) (Responsive f) (WideLayouts f)
wideNeedDisamib = Lens.reindexed (^. rWide . lForm) Lens.selfIndex . Lens.iso id (^. lWideDisambig)

hbox ::
    (MonadReader env m, Glue.HasTexts env, Applicative f) =>
    HorizDisambiguator f -> ([TextWidget f] -> [TextWidget f]) -> m (WideLayoutOption [] f)
hbox disamb spacer =
    pushToReader Glue.hbox <&>
    \box ->
    WideLayoutOption
    { _wContexts = traverse . wideNeedDisamib
    , _wLayout =
        makeWideLayouts disamb . box . spacer
        <&> Lens.mapped %~ Just
    }

wideLayoutUnambiguousOption ::
    Traversable t =>
    (t (TextWidget f) -> WideLayoutForm -> Maybe (WideLayouts f)) -> WideLayoutOption t f
wideLayoutUnambiguousOption layout =
    WideLayoutOption
    { _wContexts = traverse . wideUnambiguous
    , _wLayout = layout
    }

table ::
    ( MonadReader env m, Traversable t0, Traversable t1, Applicative f
    , Grid.Deps env
    ) =>
    m (WideLayoutOption (Compose t0 t1) f)
table =
    pushToReader Grid.make <&>
    \makeGrid ->
    wideLayoutUnambiguousOption $
    \(Compose elems) form ->
    let (alignments, gridWidget) =
            elems <&> Lens.mapped %~ toAligned & makeGrid
    in
    makeWideLayouts id
    WithTextPos
    { _textTop =
        gridWidget ^. Element.height
        * alignments ^?! traverse . traverse . Align.alignmentRatio . _2
    , _tValue = gridWidget
    } (if length elems <= 1 then form else WideMultiLine)
    & Just
    where
        toAligned (WithTextPos y w) = Aligned (Vector2 0 (y / w ^. Element.height)) w

data Disambiguators a = Disambiguators
    { _disambHoriz :: HorizDisambiguator a
    , _disambVert :: Responsive a -> Responsive a
    }

Lens.makeLenses ''Disambiguators

disambiguationNone :: Disambiguators a
disambiguationNone = Disambiguators id id

boxH ::
    (Applicative f, MonadReader env m, Glue.HasTexts env) =>
    ([TextWidget f] -> [TextWidget f]) ->
    ([Responsive f] -> [Responsive f]) ->
    Disambiguators f -> [Responsive f] -> m (Responsive f)
boxH onHGuis onVGuis disamb guis =
    do
        env <- Lens.view id
        onVGuis guis & vbox
            <&> vertLayoutMaybeDisambiguate (disamb ^. disambVert)
            <&> tryWideLayout (hbox (disamb ^. disambHoriz) onHGuis env) guis

box ::
    (Applicative f, MonadReader env m, Glue.HasTexts env) =>
    Disambiguators f -> [Responsive f] -> m (Responsive f)
box = boxH id id

boxSpaced ::
    ( Applicative f, MonadReader env m, Spacer.HasStdSpacing env
    , Glue.HasTexts env
    ) =>
    Disambiguators f -> [Responsive f] -> m (Responsive f)
boxSpaced disamb guis =
    do
        hSpace <- Spacer.stdHSpace <&> Widget.fromView <&> WithTextPos 0
        vSpace <- Spacer.stdVSpace <&> fromView
        boxH (List.intersperse hSpace) (List.intersperse vSpace) disamb guis
