-- | Draw on elements

module GUI.Momentu.Draw
    ( addInnerFrame, backgroundColor
    , alphaChannel
    , Draw.line, Draw.convexPoly
    , Draw.tint
    , Draw.Color(..), Draw.R, Image
    , Draw.openSprite, Draw.Sprite, Draw.sprite
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (R)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import qualified Graphics.DrawingCombinators.Extended as Draw

import           GUI.Momentu.Prelude

type Image = Draw.Image ()

alphaChannel :: Lens' Draw.Color R
alphaChannel f (Draw.Color r g b a) = f a <&> Draw.Color r g b

backgroundColor ::
    (MonadReader env m, Element.HasElemIdPrefix env, Element a) =>
    Draw.Color -> a -> m a
backgroundColor color w =
    Element.subElemId "bg" <&>
    \animId ->
    w & Element.setLayeredImage . Element.layers %@~ \sz -> addBg (Anim.coloredRectangle animId color & Anim.scale sz)
    where
        addBg bg [] = [bg]
        addBg bg (x:xs) = x <> bg : xs

addInnerFrame ::
    (MonadReader env m, Element.HasElemIdPrefix env, Element a) =>
    Draw.Color -> Vector2 R -> a -> m a
addInnerFrame color frameWidth w =
    Element.subElemId "inner-frame" <&>
    \animId ->
    w & Element.bottomLayer %@~ \sz -> (<>) (Anim.emptyRectangle frameWidth sz animId & Anim.unitImages %~ Draw.tint color)
