{-# LANGUAGE TemplateHaskell, BangPatterns, ConstraintKinds #-}
module GUI.Momentu.Widgets.TextView
    ( Font.Underline(..), Font.underlineColor, Font.underlineWidth
    , Style(..), styleColor, styleFont, styleUnderline, whiteText
    , HasStyle
    , color, font, underline
    , lineHeight

    , make, makeFocusable
    , Font.TextSize(..), bounding, advance
    , RenderedText(..), renderedText, renderedTextSize
    , drawText
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text.Bidi as Bidi
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (ElemId, asElemId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Font (Font, RenderedText(..), renderedText, renderedTextSize, bounding, advance)
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.DrawingCombinators.Extended as Draw

import           GUI.Momentu.Prelude

data Style = Style
    { _styleColor :: Draw.Color
    , _styleFont :: Font
    , _styleUnderline :: Maybe Font.Underline
    }
Lens.makeLenses ''Style

underline :: Has Style env => Lens' env (Maybe Font.Underline)
underline = has . styleUnderline

font :: Has Style env => Lens' env Font
font = has . styleFont

color :: Has Style env => Lens' env Draw.Color
color = has . styleColor

whiteText :: Font -> Style
whiteText f =
    Style
    { _styleColor = Draw.Color 1 1 1 1
    , _styleFont = f
    , _styleUnderline = Nothing
    }

lineHeight :: Style -> Widget.R
lineHeight s = s ^. styleFont & Font.height

fontRender :: Style -> Text -> RenderedText (Draw.Image ())
fontRender s =
    Font.render (s ^. styleFont) (s ^. styleColor) (s ^. styleUnderline) . Bidi.toVisual

type HasStyle env = (Has Dir.Layout env, Has Style env)

nestedFrame ::
    (Show a, HasStyle env) =>
    env -> (a, RenderedText (Draw.Image ())) -> RenderedText (ElemId -> Anim.Frame)
nestedFrame env (i, RenderedText size img) =
    RenderedText size draw
    where
        toFrame animId = Anim.singletonFrame anchorSize (animId <> asElemId i)
        widthV = Vector2 (size ^. bounding . _1) 0
        draw animId =
            case env ^. has of
            Dir.LeftToRight -> toFrame animId img
            Dir.RightToLeft ->
                (Draw.translateV (-widthV) Draw.%% img) & toFrame animId
                & Anim.translate widthV
        anchorSize = env ^. has & lineHeight & pure

drawText :: (MonadReader env m, HasStyle env) => Text -> m (RenderedText (ElemId -> Anim.Frame))
drawText text =
    Lens.view id <&> \env -> nestedFrame env ("text" :: Text, fontRender (env ^. has) text)

make :: (MonadReader env m, HasStyle env) => Text -> ElemId -> m (WithTextPos View)
make text animId =
    drawText text <&> \draw ->
    let RenderedText textSize frame = draw
    in  WithTextPos
        { _textTop = 0
        , _tValue = View.make (textSize ^. bounding) (frame animId)
        }

makeFocusable ::
    (MonadReader env m, Applicative f, State.HasCursor env, HasStyle env) =>
    Text -> ElemId -> m (TextWidget f)
makeFocusable text myId =
    make text myId >>= Align.tValue (Widget.makeFocusableView myId)
