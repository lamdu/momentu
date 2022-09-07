{-# LANGUAGE TemplateHaskell, ConstraintKinds #-}
{-# LANGUAGE DisambiguateRecordFields, MultiParamTypeClasses #-}
module GUI.Momentu.Widgets.Grid
    ( make
    , Keys(..), stdKeys
    , Texts(..), moreLeft, moreRight, top, bottom, leftMost, rightMost
    , englishTexts
    , HasTexts, Deps
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (msum)
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Foldable (toList)
import           Data.List.Extended.Momentu (foldl', transpose, sortOn, groupOn, minimumOn)
import           Data.MRUMemo (memo)
import           Data.Maybe.Extended (unionMaybeWith)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           GUI.Momentu.Align (Aligned(..))
import           GUI.Momentu.Direction (Orientation(..), Order(..))
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.FocusDirection (FocusDirection(..))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (OSString)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..), noMods)
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (R, Widget(Widget))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Instances as WidgetGlue
import qualified GUI.Momentu.Widgets.GridView as GridView
import           GUI.Momentu.Widgets.StdKeys (DirKeys(..))
import qualified GUI.Momentu.Widgets.StdKeys as StdKeys

import           GUI.Momentu.Prelude

data Texts a = Texts
    { _moreLeft :: a
    , _moreRight :: a
    , _top :: a
    , _bottom :: a
    , _leftMost :: a
    , _rightMost :: a
    } deriving Eq
Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts

englishTexts :: Texts Text
englishTexts = Texts
    { _moreLeft = "more left"
    , _moreRight = "more right"
    , _top = "top"
    , _bottom = "bottom"
    , _leftMost = "left-most"
    , _rightMost = "right-most"
    }

type HasTexts env = (Glue.HasTexts env, Has (Texts Text) env)

newtype Cursor = Cursor (Vector2 Int)
    deriving stock Eq
Lens.makePrisms ''Cursor

instance Field1 Cursor Cursor Int Int where _1 = _Cursor . _1
instance Field2 Cursor Cursor Int Int where _2 = _Cursor . _2

instance Ord Cursor where
    Cursor (Vector2 x0 y0) `compare` Cursor (Vector2 x1 y1) =
        compare y0 y1 <> compare x0 x1

data NavDests a = NavDests
    { cursorLeft
    , cursorUp
    , cursorRight
    , cursorDown
    , cursorTop
    , cursorLeftMost
    , cursorBottom
    , cursorRightMost :: Maybe (Widget.EnterResult a)
    }

mkNavDests ::
    Functor f =>
    Dir.Layout ->
    Cursor -> State.VirtualCursor ->
    [[Maybe (FocusDirection -> Widget.EnterResult (f State.Update))]] ->
    NavDests (f State.Update)
mkNavDests dir (Cursor (Vector2 cursorX cursorY)) virtCursor rows =
    NavDests
    { cursorLeft  =
        case dir of
        Dir.LeftToRight -> reverse colsPre
        Dir.RightToLeft -> colsPost
        & enterHoriz FromRight
    , cursorRight =
        case dir of
        Dir.LeftToRight -> colsPost
        Dir.RightToLeft -> reverse colsPre
        & enterHoriz FromLeft
    , cursorLeftMost  =
        case dir of
        Dir.LeftToRight -> colsPre
        Dir.RightToLeft -> reverse colsPost
        & take 1 & enterHoriz FromLeft
    , cursorRightMost =
        case dir of
        Dir.LeftToRight -> reverse colsPost
        Dir.RightToLeft -> colsPre
        & take 1 & enterHoriz FromRight

    , cursorUp    = reverse rowsAbove & enterVert  FromBelow
    , cursorDown  = rowsBelow         & enterVert  FromAbove

    , cursorTop       = take 1 rowsAbove           & enterVert  FromAbove
    , cursorBottom    = reverse rowsBelow & take 1 & enterVert  FromBelow
    }
    where
        columns = transpose rows
        colsPre = take cursorX columns
        colsPost = drop (cursorX+1) columns
        rowsAbove = take cursorY rows
        rowsBelow = drop (cursorY+1) rows
        enterHoriz = enterFrom Vertical Rect.verticalRange
        enterVert  = enterFrom Horizontal Rect.horizontalRange
        setVirt axis enterResult =
            enterResult
            & Widget.enterResultEvent . Lens.mapped . State.uVirtualCursor . Lens._Wrapped ?~
            ( enterResult ^. Widget.enterResultRect
                & Lens.cloneLens axis .~ prevArea ^. Lens.cloneLens axis
                & State.VirtualCursor
            )
        prevArea = virtCursor ^. State.vcRect
        enterFrom orientation axis cons lns =
            lns
            <&> foldl' (WidgetGlue.combineMEnters dir orientation) Nothing
            & msum
            ?? cons (prevArea ^# axis)
            <&> setVirt axis

data Keys key = Keys
    { _keysMoreLeft :: [key]
    , _keysMoreRight :: [key]
    , _keysLeftMost :: [key]
    , _keysRightMost :: [key]
    , _keysTop :: [key]
    , _keysBottom :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
Lens.makeLenses ''Keys
JsonTH.derivePrefixed "_keys" ''Keys

stdKeys :: OSString -> Keys ModKey
stdKeys os = Keys
    { _keysMoreLeft = [noMods ModKey.Key'Home]
    , _keysMoreRight = [noMods ModKey.Key'End]
    , _keysLeftMost = [MetaKey.cmd os ModKey.Key'Home]
    , _keysRightMost = [MetaKey.cmd os ModKey.Key'End]
    , _keysTop = [noMods ModKey.Key'PageUp]
    , _keysBottom = [noMods ModKey.Key'PageDown]
    }

type Deps env = (HasTexts env, Has (Keys ModKey) env, Has (DirKeys ModKey) env)

addNavEventmap :: Deps env => env -> NavDests a -> EventMap a -> EventMap a
addNavEventmap env navDests eMap =
    strongMap <> eMap <> weakMap
    where
        keys = env ^. has
        dir = env ^. has
        weakMap =
            [ movement Horizontal Backward (dir ^. StdKeys.keysLeft)  cursorLeft
            , movement Horizontal Forward  (dir ^. StdKeys.keysRight) cursorRight
            , movement Vertical Backward   (dir ^. StdKeys.keysUp)    cursorUp
            , movement Vertical Forward    (dir ^. StdKeys.keysDown)  cursorDown
            , movementMore (has . moreLeft ) (keys ^. keysMoreLeft)  cursorLeftMost
            , movementMore (has . moreRight) (keys ^. keysMoreRight) cursorRightMost
            ] ^. Lens.traverse . Lens._Just
        strongMap =
            [ movementMore (has . top      ) (keys ^. keysTop)       cursorTop
            , movementMore (has . bottom   ) (keys ^. keysBottom)    cursorBottom
            , movementMore (has . leftMost ) (keys ^. keysLeftMost)  cursorLeftMost
            , movementMore (has . rightMost) (keys ^. keysRightMost) cursorRightMost
            ] ^. Lens.traverse . Lens._Just
        movement o d = movementMore (has . Dir.textLens o d)
        movementMore lens events f =
            f navDests
            <&> (^. Widget.enterResultEvent)
            <&> EventMap.keyPresses
                events
                (EventMap.toDoc env
                    [ has . MomentuTexts.navigation
                    , has . MomentuTexts.move
                    , lens
                    ])

make ::
    (Traversable vert, Traversable horiz, MonadReader env m, Deps env, Applicative f) =>
    m (vert (horiz (Aligned (Widget f))) -> (vert (horiz (Aligned ())), Widget f))
make =
    Lens.view id <&>
    \env children ->
    let (size, content) = GridView.makePlacements children
    in  ( content & each2d %~ void
        , toList content <&> toList
          & each2d %~ (\(Aligned _ (rect, widget)) -> (rect, widget))
          & toWidget env size
        )

each2d :: (Traversable vert, Traversable horiz) => Lens.IndexedTraversal Cursor (vert (horiz a)) (vert (horiz b)) a b
each2d =
    Lens.traversed <.> Lens.traversed
    & Lens.reindexed (Cursor . uncurry (flip Vector2))

-- TODO: We assume that the given Cursor selects a focused
-- widget. Prove it by passing the Focused data of that widget
toWidget ::
    (Has (Keys ModKey) env, Has (DirKeys ModKey) env, HasTexts env, Applicative f) =>
    env -> Widget.Size ->
    [[(Rect, Widget f)]] ->
    Widget f
toWidget env size sChildren =
    Widget
    { _wSize = size
    , _wState =
        case translatedChildren ^@? each2d <. Widget.wState . Widget._StateFocused of
        Nothing ->
            Widget.StateUnfocused Widget.Unfocused
            { _uLayers = unfocusedLayers
            , _uMStroll = mconcat (unfocused ^.. each2d . Lens._Just . Widget.uMStroll)
            , _uMEnter = unfocusedMEnter
            }
        Just (cursor, makeFocusedChild) ->
            Widget.StateFocused $
            \surrounding ->
            let focusedChild = makeFocusedChild surrounding
                addNavDests eventContext =
                    mkNavDests dir cursor
                    (eventContext ^. Widget.eVirtualCursor) unfocusedMEnters
                    & addNavEventmap env
                (before, after) = break ((>= cursor) . fst) (sortOn fst (unfocused ^@.. each2d))
                -- TODO: DRY with Widget's Glue instance
                addEventStroll events =
                    case strollAheadDst of
                    Nothing -> events
                    Just (dst, _) ->
                        events <&> Lens.mapped %~
                        \e ->
                        if e ^. State.uPreferStroll . Lens._Wrapped
                        then
                            e
                            & State.uCursor .~ Just (dst ^. Lens._Wrapped) ^. Lens._Unwrapped
                            & State.uPreferStroll .~ mempty
                        else e
                strollDests = traverse . _2 . Lens._Just . Widget.uMStroll
                strollAheadDst = after ^. strollDests
                foldStrollDests strollKeys dirLens l =
                    foldMap
                    ( EventMap.keysEventMapMovesCursor strollKeys
                        (Glue.strollDoc env dirLens) . pure . (^. Lens.cloneLens l)
                    )
                strollBefore =
                    before ^. strollDests
                    & foldStrollDests Widget.strollBackKeys MomentuTexts.backward (_2 . Lens._Wrapped')
                strollAfter =
                    foldStrollDests Widget.strollAheadKeys MomentuTexts.forward (_1 . Lens._Wrapped') strollAheadDst
            in
            Widget.Focused
            { Widget._fLayers = focusedChild ^. Widget.fLayers <> unfocusedLayers
            , Widget._fFocalAreas = focusedChild ^. Widget.fFocalAreas
            , Widget._fPreEvents = focusedChild ^. Widget.fPreEvents
            , Widget._fMEnterPoint =
                focusedChild ^. Widget.fMEnterPoint
                & unionMaybeWith Widget.combineEnterPoints (unfocusedMEnter <&> (. Point))
            , Widget._fEventMap =
                focusedChild ^. Widget.fEventMap
                <&> addEventStroll
                & Lens.imapped %@~ addNavDests
                <&> (<> (strollBefore <> strollAfter))
            }
    }
    where
        unfocusedMEnter = combineMEnters dir unfocusedMEnters
        dir = env ^. has
        translateChildWidget (rect, widget) =
            -- Each child is set to the size of the entire grid and
            -- then translated to its place in order to fix the
            -- Surrounding parameters of all children
            Element.pad dir
            (rect ^. Rect.topLeft)
            (size - rect ^. Rect.bottomRight) widget
        translatedChildren = sChildren & each2d %~ translateChildWidget
        unfocused =
            translatedChildren
            & each2d %~ (^? Widget.wState . Widget._StateUnfocused)
        unfocusedMEnters = unfocused & each2d %~ (>>= (^. Widget.uMEnter))
        unfocusedLayers = unfocused ^. each2d . Lens._Just . Widget.uLayers

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupOn f . sortOn f

combineMEnters ::
    Dir.Layout ->
    [[Maybe (FocusDirection -> Widget.EnterResult a)]] ->
    Maybe (FocusDirection -> Widget.EnterResult a)
combineMEnters layoutDir children
    | null childEnters = Nothing
    | otherwise = Just byDirection
    where
        childEnters = children ^@.. each2d <. Lens._Just

        byDirection dir =
            filteredByEdge edge <&> (dir &) & minimumOn score
            where
                score enter
                    | dist > 0 = dist
                    | otherwise = enter ^. Widget.enterResultLayer & negate & fromIntegral
                    where
                        dist =
                            enter ^. Widget.enterResultRect
                            & Rect.distances dirRect
                            & removeUninterestingAxis
                            & Vector2.sqrNorm
                removeUninterestingAxis :: Vector2 R -> Vector2 R
                removeUninterestingAxis = ((1 - abs (fromIntegral <$> edge)) *)
                dirRect =
                    Rect 0 0 &
                    case dir of
                    Point x -> Rect.topLeft .~ x
                    FromOutside -> id
                    FromAbove x -> Rect.horizontalRange .~ x
                    FromBelow x -> Rect.horizontalRange .~ x
                    FromLeft  x -> Rect.verticalRange .~ x
                    FromRight x -> Rect.verticalRange .~ x
                edge =
                    case dir of
                    Point{} -> Vector2 0 0 -- Check all widgets for mouse movements (for hovers)
                    FromOutside -> Vector2 0 0
                    FromAbove{} -> Vector2 0 (-1)
                    FromBelow{} -> Vector2 0 1
                    FromLeft{}  -> Vector2 (negate afterEdge) 0
                    FromRight{} -> Vector2 afterEdge 0
                afterEdge =
                    case layoutDir of
                    Dir.LeftToRight -> 1
                    Dir.RightToLeft -> -1

        -- | Take only the first/last enterable row/column
        filteredByEdge =
            memo $ \(Vector2 hEdge vEdge) ->
            childEnters
            & groupSortOn ((* (-vEdge)) . (^._1._2)) & (^. Lens.ix 0)
            & groupSortOn ((* (-hEdge)) . (^._1._1)) & (^. Lens.ix 0)
            <&> snd
