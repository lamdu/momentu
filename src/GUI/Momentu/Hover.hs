{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE RankNTypes, DerivingVia #-}
module GUI.Momentu.Hover
    ( Style(..), frameColor, framePadding, bgColor, bgPadding
        , defaultStyle
    , Hover, hover, sequenceHover
    , backgroundColor
    , AnchoredWidget, anchor
    , hoverInPlaceOf, hoverBesideOptions
    , Ordered(..), forward, backward
    , hoverBesideOptionsAxis
    , Orientation(..)
    , hoverBeside

    , -- For tests
      anchorPoint, unHover
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.List.Extended.Momentu (minimumOn)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), value)
import           GUI.Momentu.Direction (Orientation(..), Order(..))
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Glue, GluesTo)
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget(..), R)
import qualified GUI.Momentu.Widget as Widget

import           GUI.Momentu.Prelude

data Style = Style
    { _frameColor :: Draw.Color
    , _framePadding :: Vector2 R
    , _bgColor :: Draw.Color
    , _bgPadding :: Vector2 R
    } deriving (Eq, Generic, Show)
JsonTH.derivePrefixed "_" ''Style

defaultStyle :: Style
defaultStyle =
    Style
    { _frameColor = Draw.Color 0.17 0.18 0.18 1.0
    , _bgColor    = Draw.Color 0.13 0.22 0.26 1.0
    , _framePadding = Vector2 1 1
    , _bgPadding    = Vector2 7 7
    }

Lens.makeLenses ''Style

backgroundColor :: Has Style env => Lens' env Draw.Color
backgroundColor = has . bgColor

data AnchoredWidget f = AnchoredWidget
    { _anchorPoint :: Vector2 R
    , _anchored :: Widget f
    }
Lens.makeLenses ''AnchoredWidget

newtype Hover a = Hover { _unHover :: a }
Lens.makeLenses ''Hover

instance Element a => Element (Hover a) where
    setLayeredImage = unHover . Element.setLayeredImage
    hoverLayeredImage = unHover %~ Element.hoverLayeredImage
    padImpl p0 p1 = unHover %~ Element.padImpl p0 p1
    scale r = unHover %~ Element.scale r
    empty = Hover Element.empty

instance SizedElement a => SizedElement (Hover a) where
    size = unHover . Element.size

instance Functor f => Element (AnchoredWidget f) where
    setLayeredImage = anchored . Element.setLayeredImage
    hoverLayeredImage = anchored %~ Element.hoverLayeredImage
    empty = AnchoredWidget 0 Element.empty
    padImpl tl br (AnchoredWidget point w) =
        AnchoredWidget
        { _anchorPoint = point + tl
        , _anchored = Element.padImpl tl br w
        }
    scale ratio (AnchoredWidget point w) =
        AnchoredWidget
        { _anchorPoint = point * ratio
        , _anchored = Element.scale ratio w
        }

instance Functor f => SizedElement (AnchoredWidget f) where
    size = anchored . Element.size

instance
    ( Functor f, Has Dir.Layout env
    ) => Glue env (AnchoredWidget f) (Hover View) where
    type Glued (AnchoredWidget f) (Hover View) = Hover (AnchoredWidget f)
    glue env o ow (Hover ov) = Glue.glue env o ow ov & Hover

instance
    ( Functor f, Has Dir.Layout env
    ) => Glue env (Hover View) (AnchoredWidget f) where
    type Glued (Hover View) (AnchoredWidget f) = Hover (AnchoredWidget f)
    glue env o (Hover ov) ow = Glue.glue env o ov ow & Hover

instance
    ( Functor f, Has Dir.Layout env
    ) => Glue env (AnchoredWidget f) View where
    type Glued (AnchoredWidget f) View = AnchoredWidget f
    glue =
        Glue.glueH f
        where
            f w v = w & Element.setLayeredImage <>~ v ^. View.vAnimLayers

instance
    ( Functor f, Has Dir.Layout env
    ) => Glue env View (AnchoredWidget f) where
    type Glued View (AnchoredWidget f) = AnchoredWidget f
    glue =
        Glue.glueH f
        where
            f v w = w & Element.setLayeredImage <>~ v ^. View.vAnimLayers

instance
    ( Applicative f, f ~ g, Glue.HasTexts env
    ) => Glue env (AnchoredWidget f) (Hover (Widget g)) where
    type Glued (AnchoredWidget f) (Hover (Widget g)) = Hover (AnchoredWidget f)
    glue env orientation ow0 (Hover ow1) =
        Glue.glueH f env orientation ow0 ow1 & Hover
        where
            f (AnchoredWidget pos w0) w1 =
                Widget.glueStates env orientation Forward Forward w0 w1
                & AnchoredWidget pos

instance
    ( Applicative f, f ~ g, Glue.HasTexts env
    ) => Glue env (Hover (Widget f)) (AnchoredWidget g) where
    type Glued (Hover (Widget f)) (AnchoredWidget g) =
        Hover (AnchoredWidget f)
    glue env orientation (Hover ow0) =
        Glue.glueH f env orientation ow0 <&> Hover
        where
            f w0 (AnchoredWidget pos w1) =
                -- The hover is always logically "after" the
                -- lower-layer widgets, no matter if it is glued
                -- before/after geometrically
                Widget.glueStates env orientation Forward Backward w0 w1
                & AnchoredWidget pos

data Ordered a = Ordered
    { _forward :: a
    , _backward :: a
    } deriving stock (Generic, Generic1, Functor, Foldable, Traversable)
    deriving Applicative via Generically1 Ordered
Lens.makeLenses ''Ordered

hoverBesideOptionsAxis ::
    ( MonadReader env m, GluesTo env a b c, Has Dir.Layout env
    , SizedElement a, SizedElement b, SizedElement c
    ) =>
    Orientation -> Ordered a -> b -> m [c]
hoverBesideOptionsAxis o (Ordered fwd bwd) src =
    Glue.mkPoly o <&> \(Glue.Poly glue) ->
    do
        x <- [0, 1]
        let aSrc = Aligned x src
        [ glue aSrc (Aligned x fwd)
            , glue (Aligned x bwd) aSrc]
            <&> (^. value)

anchor ::
    (MonadReader env m, Has Dir.Layout env) =>
    Widget a -> m (AnchoredWidget a)
anchor w =
    Lens.view has
    <&>
    \case
    Dir.LeftToRight -> AnchoredWidget 0 w
    Dir.RightToLeft -> AnchoredWidget (Vector2 (w ^. Widget.wSize . _1) 0) w

hoverBesideOptions ::
    ( MonadReader env m, GluesTo env a b c, Has Dir.Layout env
    , SizedElement a, SizedElement b, SizedElement c
    ) =>
    a -> b -> m [c]
hoverBesideOptions h src =
    Lens.view id <&>
    \env ->
    do
        o <- [Vertical, Horizontal]
        hoverBesideOptionsAxis o (Ordered h h) src env

addFrame ::
    (MonadReader env m, Has Style env, SizedElement a, Element.HasElemIdPrefix env) =>
    a -> m a
addFrame gui
    | gui ^. Element.size == 0 = pure gui
    | otherwise =
        do
            s <- Lens.view has
            bgId <- Element.subElemId "hover bg"
            frameId <- Element.subElemId "hover frame"
            pure $
                gui
                & Element.padAround (s ^. bgPadding)
                & (Draw.backgroundColor (s ^. bgColor) ?? bgId)
                & Element.padAround (s ^. framePadding)
                & (Draw.backgroundColor (s ^. frameColor) ?? frameId)

hover ::
    (MonadReader env m, SizedElement a, Has Style env, Element.HasElemIdPrefix env) =>
    a -> m (Hover a)
hover widget = addFrame widget <&> Element.hoverLayeredImage <&> Hover

sequenceHover :: Functor f => Hover (f a) -> f (Hover a)
sequenceHover (Hover x) = x <&> Hover

emplaceAt ::
    (Functor f, Functor g) =>
    AnchoredWidget f ->
    AnchoredWidget g ->
    Widget f
emplaceAt h place =
    Element.padImpl translation 0 (h ^. anchored)
    & Widget.wSize .~ place ^. Element.size
    where
        translation = place ^. anchorPoint - h ^. anchorPoint

-- TODO: Second argument here is really only (anchorPoint,size), take
-- it as such?
hoverInPlaceOf ::
    (Functor f, Functor g) =>
    [Hover (AnchoredWidget f)] ->
    AnchoredWidget g -> Widget f
hoverInPlaceOf [] _ = error "no hover options!"
hoverInPlaceOf hoverOptions@(Hover defaultOption:_) place
    | null focusedOptions = defaultOption `emplaceAt` place
    | otherwise =
        Widget
        { Widget._wSize = place ^. Element.size
        , Widget._wState = Widget.StateFocused makeFocused
        }
    where
        translation h = place ^. anchorPoint - h ^. anchorPoint
        -- All hovers *should* be same the - either focused or unfocused..
        focusedOptions =
            do
                Hover x <- hoverOptions
                mkFocused <- x ^.. anchored . Widget.wState . Widget._StateFocused
                pure (x, mkFocused)
        makeFocused surrounding =
            surrounding
            & Widget.sRight -~ sizeDiff ^. _1
            & Widget.sBottom -~ sizeDiff ^. _2
            & Widget.translateFocused (translation h) hMakeFocused
            & Widget.fFocalAreas %~ (Rect 0 (place ^. Element.size) :)
            where
                (h, hMakeFocused) = pickOption surrounding
                sizeDiff = h ^. Element.size - place ^. Element.size
        pickOption surrounding = minimumOn (negate . remainSurrouding surrounding . (^. _1)) focusedOptions
        remainSurrouding surrounding h =
            filter (>= 0)
            [ surrounding ^. Widget.sLeft - tl ^. _1
            , surrounding ^. Widget.sTop - tl ^. _2
            , surrounding ^. Widget.sRight - br ^. _1
            , surrounding ^. Widget.sBottom - br ^. _2
            ]
            & length
            where
                tl = negate (translation h)
                br = h ^. Element.size - place ^. Element.size - tl

hoverBeside ::
    ( GluesTo env (Hover w) (AnchoredWidget f) (Hover (AnchoredWidget f))
    , MonadReader env m, Functor f, SizedElement w
    , Element.HasElemIdPrefix env, Has Style env, Has Dir.Layout env
    ) =>
    (forall a b. Lens (t a) (t b) a b) ->
    t (Widget f) ->
    w ->
    m (t (Widget f))
hoverBeside lens layout h =
    do
        a <- lens anchor layout
        b <- hover h >>= (`hoverBesideOptions` (a ^. lens))
        a & lens %~ hoverInPlaceOf b & pure
