module GUI.Momentu.Widget.Events
    ( addPreEventToEventMap, combineEnterPoints, combineMEnters
    ) where

import qualified Control.Lens as Lens
import           Graphics.DrawingCombinators (R)
import           Data.Maybe.Extended (unionMaybeWith)
import qualified Data.Text as Text
import           Data.Vector.Vector2
import           GUI.Momentu.Direction (Orientation(..), Layout (..))
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.FocusDirection (FocusDirection(..))
import           GUI.Momentu.Rect (Rect)
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.Widget.Types as Types

import           GUI.Momentu.Prelude

-- | Takes a manual `mappend` function to avoid needing "Monoid (f a)"
-- constraint in callers, who can give the Applicative-Monoid instance
-- for a generic Applicative without requiring a cumbersome
-- "Applicative (f a)" constraint
addPreEventToEventMap :: (a -> a -> a) -> PreEvent a -> EventMap a -> EventMap a
addPreEventToEventMap append preEvent e =
    e
    & actionText %~ concatDescs (preEvent ^. pDesc)
    <&> append (preEvent ^. pAction)
    where
        actionText = E.emHandlerDocHandlers . E.dhDoc . E.docStrs . Lens.reversed . Lens.element 0
        concatDescs x y = filter (not . Text.null) [x, y] & Text.intercalate ", "

combineEnterPoints ::
    (Vector2 R -> EnterResult a) -> (Vector2 R -> EnterResult a) ->
    Vector2 R -> EnterResult a
combineEnterPoints e0 e1 p = closerGeometric p (e0 p) (e1 p)

closerGeometric :: Vector2 R -> EnterResult a -> EnterResult a -> EnterResult a
closerGeometric p r0 r1
    | Rect.sqrPointDistance p (r0 ^. enterResultRect) <=
      Rect.sqrPointDistance p (r1 ^. enterResultRect) = r0
    | otherwise = r1

combineMEnters ::
    Has Layout env =>
    env -> Orientation ->
    Maybe (FocusDirection -> EnterResult a) ->
    Maybe (FocusDirection -> EnterResult a) ->
    Maybe (FocusDirection -> EnterResult a)
combineMEnters env = unionMaybeWith . combineEnters (env ^. has)

combineEnters ::
    Layout -> Orientation ->
    (FocusDirection -> EnterResult a) ->
    (FocusDirection -> EnterResult a) ->
    FocusDirection -> EnterResult a
combineEnters ldir o e0 e1 dir = chooseEnter ldir o dir (e0 dir) (e1 dir)

chooseEnter ::
    Layout -> Orientation -> FocusDirection ->
    EnterResult a -> EnterResult a -> EnterResult a
chooseEnter _ _          FromOutside r0 _  = r0 -- left-biased
chooseEnter _ _          (Point p) r0 r1 = closerGeometric p r0 r1
chooseEnter _ Vertical   FromAbove{} r0 _  = r0
chooseEnter _ Vertical   FromBelow{} _  r1 = r1
chooseEnter _ Horizontal (FromAbove r) r0 r1 =
    closer Rect.horizontalRange r r0 r1
chooseEnter _ Horizontal (FromBelow r) r0 r1 =
    closer Rect.horizontalRange r r0 r1
chooseEnter _ Vertical (FromLeft r) r0 r1 =
    closer Rect.verticalRange r r0 r1
chooseEnter _ Vertical (FromRight r) r0 r1 =
    closer Rect.verticalRange r r0 r1
chooseEnter LeftToRight Horizontal FromLeft{}  r0 _  = r0
chooseEnter LeftToRight Horizontal FromRight{} _  r1 = r1
chooseEnter RightToLeft Horizontal FromLeft{}  _  r1 = r1
chooseEnter RightToLeft Horizontal FromRight{} r0 _  = r0

closer ::
    Lens.ALens' Rect (Rect.Range R) -> Rect.Range R ->
    EnterResult a -> EnterResult a -> EnterResult a
closer axis r r0 r1
    | Rect.rangeDistance r (r0 ^# enterResultRect . axis) <=
      Rect.rangeDistance r (r1 ^# enterResultRect . axis) = r0
    | otherwise = r1
