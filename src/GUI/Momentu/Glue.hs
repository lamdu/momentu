{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module GUI.Momentu.Glue
    ( Texts(..), stroll
        , strollDoc
    , englishTexts
    , HasTexts
    , Glue(..), GluesTo
    , (/|/), (/-/)
    , box, hbox, vbox
    , glueH
    , mkGlue
    , Orientation(..)
    , Poly(..), mkPoly
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Foldable (foldrM)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Direction (Orientation(..), axis, perpendicular)
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as EventMap
import qualified GUI.Momentu.I18N as MomentuTexts

import           GUI.Momentu.Prelude

newtype Texts a = Texts
    { _stroll :: a
    } deriving stock Eq

Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts

englishTexts :: Texts Text
englishTexts = Texts
    { _stroll = "Stroll"
    }

type HasTexts env =
    ( Has (Dir.Texts Text) env
    , Has Dir.Layout env
    , Has (Texts Text) env
    , Has (MomentuTexts.Texts Text) env
    )

strollDoc :: HasTexts env => env -> Lens.ALens' (MomentuTexts.Texts Text) Text -> EventMap.Doc
strollDoc env dirLens =
    EventMap.Doc
    [ env ^. has . MomentuTexts.navigation
    , env ^# has . stroll
    , env ^# has . dirLens
    ]

class (Glued b a ~ Glued a b) => Glue env a b where
    type Glued a b
    glue :: env -> Orientation -> a -> b -> Glued a b

type GluesTo env a b c = (Glue env a b, Glue env b a, Glued a b ~ c)

newtype Poly env = Poly { polyGlue :: forall a b. Glue env a b => a -> b -> Glued a b }

mkPoly :: MonadReader env m => Orientation -> m (Poly env)
mkPoly orientation = Lens.view id <&> \env -> Poly (glue env orientation)

mkGlue ::
    (MonadReader env m, Glue env a b) =>
    Orientation -> a -> b -> m (Glued a b)
mkGlue orientation l r = Lens.view id <&> glue ?? orientation ?? l ?? r

-- Horizontal glue
(/|/) ::
    (MonadReader env m, Glue env a b) =>
    m a -> m b -> m (Glued a b)
mkL /|/ mkR =
    do
        l <- mkL
        mkR >>= mkGlue Horizontal l

-- Vertical glue
(/-/) ::
    (MonadReader env m, Glue env a b) =>
    m a -> m b -> m (Glued a b)
mkL /-/ mkR =
    do
        l <- mkL
        mkR >>= mkGlue Vertical l

glueH ::
    (SizedElement a, SizedElement b, Has Dir.Layout env) =>
    (a -> b -> c) -> env -> Orientation -> a -> b -> c
glueH f direction orientation v0 v1 =
    f
    (Element.pad v0pre v0post v0 direction)
    (Element.pad v1pre v1post v1 direction)
    where
        v0pre = 0
        v0post = v1s & perp -~ v0s ^. perp & perp %~ max 0
        v1pre = v0s & perp .~ 0
        v1post = v0s & ax .~ 0 & perp -~ v1s ^. perp & perp %~ max 0
        ax = axis orientation
        perp :: Lens' (Vector2 a) a
        perp = axis (perpendicular orientation)
        v0s = v0 ^. Element.size
        v1s = v1 ^. Element.size

box ::
    (Element a, GluesTo env a a a, MonadReader env m, Foldable t) =>
    Orientation -> t a -> m a
box orientation = foldrM (mkGlue orientation) Element.empty

hbox ::
    (Element a, GluesTo env a a a, MonadReader env m, Foldable t) =>
    t a -> m a
hbox = box Horizontal

vbox ::
    (Element a, GluesTo env a a a, MonadReader env m, Foldable t) =>
    t a -> m a
vbox = box Vertical
