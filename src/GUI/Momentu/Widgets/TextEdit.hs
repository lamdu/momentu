{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns, NamedFieldPuns, RankNTypes #-}
{-# LANGUAGE DerivingVia, MultiParamTypeClasses, ConstraintKinds #-}
module GUI.Momentu.Widgets.TextEdit
    ( Style(..)
        , sCursorColor, sCursorWidth, sEmptyStringsColors, sTextViewStyle
    , Modes(..), focused, unfocused
    , EmptyStrings
    , make, makeWithAnimId
    , defaultStyle
    , getCursor, encodeCursor
    , Texts(..)
        , textTill, textNewline, textSpace, textBeginningOfLine, textEndOfLine
        , textBeginningOfText, textEndOfText, textSwapLetters, textCharacter
        , textClipboard, textPaste, textCopy, textWord
    , englishTexts
    , Keys(..)
        , keysDir, keysMoveLeftWord, keysMoveRightWord
        , keysDeleteCharBackward, keysDeleteCharForward, keysDeleteWordBackward
        , keysCopy, keysPaste
    , stdKeys
    , Deps
    , OSString
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified Data.Binary.Extended as Binary
import           Data.Char (isSpace)
import           Data.List.Extended (genericLength, minimumOn)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup (Max(..), sconcat)
import qualified Data.Text as Text
import qualified Data.Text.Bidi as Bidi
import qualified Data.Vector.Unboxed as Vector
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (TextWidget, WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.FocusDirection (FocusDirection(..))
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.MetaKey (OSString, cmd)
import           GUI.Momentu.ModKey (ModKey(..), noMods, ctrl, alt)
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widgets.StdKeys (DirKeys)
import qualified GUI.Momentu.Widgets.StdKeys as StdKeys
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Graphics.DrawingCombinators as Draw

import           GUI.Momentu.Prelude

data Texts a = Texts
    { _textWord :: a
    , _textTill :: a
    , _textNewline :: a
    , _textSpace :: a
    , _textBeginningOfLine :: a
    , _textEndOfLine :: a
    , _textBeginningOfText :: a
    , _textEndOfText :: a
    , _textSwapLetters :: a
    , _textCharacter :: a
    , _textClipboard :: a
    , _textPaste :: a
    , _textCopy :: a
    } deriving Eq
JsonTH.derivePrefixed "_text" ''Texts

englishTexts :: Texts Text
englishTexts = Texts
    { _textWord = "Word"
    , _textTill = "Till"
    , _textNewline = "Newline"
    , _textSpace = "Space"
    , _textBeginningOfLine = "Beginning Of Line"
    , _textEndOfLine = "End Of Line"
    , _textBeginningOfText = "Beginning Of Text"
    , _textEndOfText = "End Of Text"
    , _textSwapLetters = "Swap Letters"
    , _textCharacter = "Characters"
    , _textClipboard = "Clipboard"
    , _textPaste = "Paste"
    , _textCopy = "Copy"
    }

type HasTexts env =
    ( Has (Dir.Texts Text) env, Has Dir.Layout env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts Text) env
    )

Lens.makeLenses ''Texts

type Cursor = Int

data Modes a = Modes
    { _unfocused :: a
    , _focused :: a
    } deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via Generically1 Modes
Lens.makeLenses ''Modes
JsonTH.derivePrefixed "_" ''Modes

toNonEmpty :: Modes a -> NonEmpty a
toNonEmpty (Modes u f) = u :| [f]

type EmptyStrings = Modes Text

data Style = Style
    { _sCursorColor :: Draw.Color
    , _sCursorWidth :: Widget.R
    , _sEmptyStringsColors :: Modes Draw.Color
    , _sTextViewStyle :: TextView.Style
    }
Lens.makeLenses ''Style

data Keys key = Keys
    { _keysDir :: DirKeys key
    , _keysMoveLeftWord :: [key]
    , _keysMoveRightWord :: [key]

    , _keysHome :: [key]
    , _keysEnd :: [key]

    , _keysTransposeLetters :: [key]

    , _keysDeleteCharBackward :: [key]
    , _keysDeleteCharForward :: [key]
    , _keysDeleteWordBackward :: [key]
    , _keysDeleteWordForward :: [key]
    , _keysDeleteToEndOfLine :: [key]
    , _keysDeleteToBeginningOfLine :: [key]

    -- clipboard
    , _keysCopy :: [key]
    , _keysPaste :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable)
Lens.makeLenses ''Keys
JsonTH.derivePrefixed "_keys" ''Keys

stdKeys :: OSString -> Keys ModKey
stdKeys os = Keys
    { _keysDir                     = StdKeys.stdDirKeys <&> noMods
    , _keysMoveLeftWord            = [ctrl ModKey.Key'Left]
    , _keysMoveRightWord           = [ctrl ModKey.Key'Right]

    , _keysHome                    = [noMods ModKey.Key'Home, ctrl ModKey.Key'A]
    , _keysEnd                     = [noMods ModKey.Key'End, ctrl ModKey.Key'E]

    , _keysTransposeLetters        = [ctrl ModKey.Key'T]

    , _keysDeleteCharBackward      = [noMods ModKey.Key'Backspace]
    , _keysDeleteCharForward       = [noMods ModKey.Key'Delete]
    , _keysDeleteWordBackward      = [ctrl ModKey.Key'W]
    , _keysDeleteWordForward       = [alt ModKey.Key'D]
    , _keysDeleteToEndOfLine       = [ctrl ModKey.Key'K]
    , _keysDeleteToBeginningOfLine = [ctrl ModKey.Key'U]

    -- clipboard
    , _keysCopy                    = [cmd os ModKey.Key'C]
    , _keysPaste                   = [cmd os ModKey.Key'V]
    }

type HasStyle env = (Has Style env, TextView.HasStyle env)

type Deps env = (State.HasCursor env, HasStyle env, HasTexts env, Has (Keys ModKey) env)

instance Has TextView.Style Style where has = sTextViewStyle

defaultStyle :: TextView.Style -> Style
defaultStyle tvStyle =
    Style
    { _sCursorColor = Draw.Color 0 1 0 1
    , _sEmptyStringsColors = pure (Draw.Color r g b a)
    , _sCursorWidth = 4
    , _sTextViewStyle = tvStyle
    }
    where
        Draw.Color r g b a = tvStyle ^. TextView.color

tillEndOfWord :: Text -> Text
tillEndOfWord xs = spaces <> nonSpaces
    where
        spaces = Text.takeWhile isSpace xs
        nonSpaces = Text.dropWhile isSpace xs & Text.takeWhile (not . isSpace)

encodeCursor :: Widget.Id -> Cursor -> Widget.Id
encodeCursor myId = Widget.joinId myId . (:[]) . Binary.encodeS

-- | Returns at least one rect
letterRects :: Has TextView.Style env => env -> Text -> [[Rect]]
letterRects env text =
    zipWith locateLineHeight (iterate (+ height) 0) textLines
    where
        -- splitOn returns at least one string:
        textLines = Text.splitOn "\n" text <&> makeLine
        locateLineHeight y = Lens.mapped . Rect.top +~ y
        font = env ^. has . TextView.styleFont
        height = TextView.lineHeight (env ^. has)
        makeLine textLine =
            zipWith letterRect boundingSizes (0 : advances)
            where
                advances = Font.textPositions font textLine & Vector.toList
                letterRect boundingSize xpos = Rect (Vector2 xpos 0) boundingSize
                boundingSizes =
                    Text.unpack textLine <&> Text.singleton <&> Font.textSize font
                    <&> (^. Font.bounding)

cursorRects :: HasStyle env => env -> Text -> [Rect]
cursorRects env str =
    letterRects env str
    <&> Lens.mapped %~ rightSideOfRect
    & zipWith addFirstCursor (iterate (+lineHeight) 0)
    -- A bit ugly: letterRects returns rects for all but newlines, and
    -- returns a list of lines. Then addFirstCursor adds the left-most
    -- cursor of each line, thereby the number of rects becomes the
    -- original number of letters which can be used to match the
    -- original string index-wise.
    & concat
    where
        rightSideOfRect rect =
            rect
            & Rect.left .~ rect ^. Rect.right
            & Rect.width .~ cursorWidth
        cursorWidth = env ^. has . sCursorWidth
        addFirstCursor y = (Rect (Vector2 0 y) (Vector2 cursorWidth lineHeight) :)
        lineHeight = TextView.lineHeight (env ^. has)

mkView :: (Has Dir.Layout env, Has TextView.Style env, Has Style s) =>
                s -> [ByteString] -> Text -> (s -> env) -> WithTextPos View
mkView env animId displayStr setColor =
    TextView.make (setColor env) displayStr animId
    & Element.padAround (Vector2 (env ^. has . sCursorWidth / 2) 0)

-- A debugging facility, showing all the potential locations of all cursors
_drawCursorRects :: HasStyle env => Anim.AnimId -> env -> Text -> Anim.Frame
_drawCursorRects animId env str =
    cursorRects env str
    & Lens.traversed %@~ drawRect
    & mconcat
    where
        drawRect i rect =
            Anim.augmentId i (animId ++ ["text-cursor"])
            & Anim.unitSquare
            -- & Anim.unitImages %~ Draw.tint (Draw.Color 1 1 1 1)
            & Anim.scale (rect ^. Rect.size)
            & Anim.translate (rect ^. Rect.topLeft)

_addCursorRects :: (Element t, HasStyle env) => env -> Anim.AnimId -> Text -> t -> t
_addCursorRects env animId str =
    Element.setLayeredImage . Element.layers %~ (_drawCursorRects animId env str :)

makeInternal ::
    HasStyle env =>
    env -> (forall a. Lens.Getting a (Modes a) a) ->
    Text -> Modes (WithTextPos View) -> Anim.AnimId -> Widget.Id ->
    TextWidget ((,) Text)
makeInternal env mode str emptyViews animId myId =
    v
    -- TODO: Control with debug config variable?
    -- & Align.tValue %~ _addCursorRects env animId str
    & Align.tValue %~ Widget.fromView
    & Align.tValue . Widget.wState . Widget._StateUnfocused . Widget.uMEnter ?~
        Widget.enterFuncAddVirtualCursor rect
        (enterFromDirection env rect str myId)
    where
        rect = Rect 0 (v ^. Element.size)
        v
            | Text.null str = emptyViews ^. mode
            | otherwise = mkView env animId (Text.take 5000 str) id

minimumIndex :: Ord a => [a] -> Int
minimumIndex xs =
    xs ^@.. Lens.traversed & minimumOn snd & fst

cursorNearRect :: HasStyle env => env -> Text -> Rect -> Cursor
cursorNearRect env str fromRect =
    cursorRects env str <&> Rect.sqrDistance fromRect
    & minimumIndex -- cursorRects(TextView.letterRects) should never return an empty list

enterFromDirection ::
    HasStyle env =>
    env -> Rect -> Text -> Widget.Id ->
    FocusDirection -> Widget.EnterResult (Text, State.Update)
enterFromDirection env rect str myId dir =
    encodeCursor myId cursor
    & State.updateCursor
    & (,) str
    & Widget.EnterResult rect 0
    where
        cursor =
            case dir of
            Point x -> Rect x 0 & fromRect
            FromOutside -> Text.length str
            FromLeft  r -> Rect 0 0    & Rect.verticalRange   .~ r & fromRect
            FromRight r -> edgeRect _1 & Rect.verticalRange   .~ r & fromRect
            FromAbove r -> Rect 0 0    & Rect.horizontalRange .~ r & fromRect
            FromBelow r -> edgeRect _2 & Rect.horizontalRange .~ r & fromRect
        edgeRect l = Rect (0 & Lens.cloneLens l .~ rect ^. Rect.size . Lens.cloneLens l) 0
        width = rect ^. Rect.size . _1
        needsInvert
            | Text.null str = env ^. has == Dir.RightToLeft
            | otherwise = not (Bidi.isLeftToRight str)
        maybeInvert
            | needsInvert = Rect.horizontalRange . Rect.rangeStart %~ (width -)
            | otherwise = id
        fromRect = cursorNearRect env str . maybeInvert

eventResult :: Widget.Id -> Text -> Cursor -> (Text, State.Update)
eventResult myId newText newCursor =
    ( newText
    , encodeCursor myId newCursor & State.updateCursor
    )

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
makeFocused ::
    Deps env =>
    env -> Text -> EmptyStrings -> Modes (WithTextPos View) -> Cursor -> Anim.AnimId -> Widget.Id ->
    TextWidget ((,) Text)
makeFocused env str emptyStr emptyViews cursor animId myId =
    makeInternal env focused str emptyViews animId myId
    & Element.bottomLayer <>~ cursorFrame
    & Align.tValue %~
        Widget.setFocusedWith cursorRect
        (eventMap env cursor str myId)
    where
        actualStr = if Text.null str then emptyStr ^. focused else str
        cursorRect@(Rect origin size) = mkCursorRect env cursor actualStr
        cursorFrame =
            Anim.unitSquare ["text-cursor"]
            & Anim.unitImages %~ Draw.tint (env ^. has . sCursorColor)
            & unitIntoCursorRect
        unitIntoCursorRect img =
            img
            & Anim.scale size
            & Anim.translate origin

mkCursorRect :: HasStyle env => env -> Cursor -> Text -> Rect
mkCursorRect env cursor str =
    Rect cursorPos cursorSize
    where
        beforeCursorLines = Text.splitOn "\n" $ Text.take cursor str
        lineHeight = TextView.lineHeight (env ^. has . sTextViewStyle)
        maybeMirror =
            case env ^. has of
            Dir.LeftToRight -> id
            Dir.RightToLeft -> (totalWidth -)
        cursorPos = Vector2 (maybeMirror cursorPosX) cursorPosY
        cursorSize = Vector2 cursorWidth lineHeight
        cursorWidth = env ^. has . sCursorWidth
        totalWidth = draw str ^. TextView.renderedTextSize . Font.bounding . _1
        draw = TextView.drawText env
        cursorPosX =
            draw (last beforeCursorLines) ^.
            TextView.renderedTextSize . Font.advance . _1
        cursorPosY = lineHeight * (genericLength beforeCursorLines - 1)

-- TODO: Implement intra-TextEdit virtual cursor
eventMap ::
    (HasTexts env, Has (Keys ModKey) env) =>
    env -> Cursor -> Text -> Widget.Id -> Widget.EventContext ->
    EventMap (Text, State.Update)
eventMap env cursor str myId _eventContext =
    mconcat $ concat [
        [ moveRelative (-1) & logicalRetreat | cursor > 0 ],
        [ moveRelative   1  & logicalAdvance | cursor < textLength ],
        [ logicalRetreatWord backMoveWord | cursor > 0 ],
        [ logicalAdvanceWord moveWord | cursor < textLength ],

        [ moveRelative (- cursorX - 1 - Text.length (Text.drop cursorX prevLine))
            & E.keyPressesOrRepeat (keys ^. keysDir . StdKeys.keysUp)
            (moveDoc [has . Dir.up])
        | cursorY > 0 ],

        [ moveRelative
            (Text.length curLineAfter + 1 + min cursorX (Text.length nextLine))
            & E.keyPressesOrRepeat (keys ^. keysDir . StdKeys.keysDown)
            (moveDoc [has . Dir.down])
        | cursorY < lineCount - 1 ],

        [ moveRelative (-cursorX)
            & keyPresses (moveDoc [has . textBeginningOfLine]) (keys ^. keysHome)
        | cursorX > 0 ],

        [ moveRelative (Text.length curLineAfter)
            & keyPresses (moveDoc [has . textEndOfLine]) (keys ^. keysEnd)
        | not . Text.null $ curLineAfter ],

        [ moveAbsolute 0 & keyPresses (moveDoc [has . textBeginningOfText]) (keys ^. keysHome)
        | cursorX == 0 && cursor > 0 ],

        [ moveAbsolute textLength
            & keyPresses (moveDoc [has . textEndOfText]) (keys ^. keysEnd)
        | Text.null curLineAfter && cursor < textLength ],

        [ backDelete 1
            & keyPresses (deleteDoc [has . MomentuTexts.backward]) (keys ^. keysDeleteCharBackward)
        | cursor > 0 ],

        [ keyPresses (deleteDoc [has . textWord, has . MomentuTexts.backward])
            (keys ^. keysDeleteWordBackward) backDeleteWord
        | cursor > 0 ],

        let swapPoint = min (textLength - 2) (cursor - 1)
            (beforeSwap, Text.unpack -> x:y:afterSwap) = Text.splitAt swapPoint str
            swapLetters =
                min textLength (cursor + 1)
                & eventRes (beforeSwap <> Text.pack (y:x:afterSwap))

        in

        [ keyPresses (editDoc [has . textSwapLetters]) (keys ^. keysTransposeLetters)
            swapLetters
        | cursor > 0 && textLength >= 2 ],

        [ delete 1
            & keyPresses (deleteDoc [has . MomentuTexts.forward]) (keys ^. keysDeleteCharForward)
        | cursor < textLength ],

        [ keyPresses (deleteDoc [has . textWord, has . MomentuTexts.forward]) (keys ^. keysDeleteWordForward)
            deleteWord
        | cursor < textLength ],

        [ delete (Text.length curLineAfter)
            & keyPresses (deleteDoc [has . textTill, has . textEndOfLine])
            (keys ^. keysDeleteToEndOfLine)
        | not . Text.null $ curLineAfter ],

        [ delete 1 & keyPresses (deleteDoc [has . textNewline]) (keys ^. keysDeleteToEndOfLine)
        | Text.null curLineAfter && cursor < textLength ],

        [ backDelete (Text.length curLineBefore)
            & keyPresses (deleteDoc [has . textTill, has . textBeginningOfLine])
            (keys ^. keysDeleteToBeginningOfLine)
        | not . Text.null $ curLineBefore ],

        [ E.filterChars (`notElem` (" \n" :: String)) .
            E.allChars "Character" (insertDoc [has . textCharacter]) $
            insert . Text.singleton
        ],

        [ keyPresses (insertDoc [has . textNewline]) insertNewlineKeys (insert "\n") ],
        [ keyPresses (insertDoc [has . textSpace]) insertSpaceKeys (insert " ") ],

        [ foldMap E.pasteOnKey (keys ^. keysPaste)
            (toDoc [has . textClipboard, has . textPaste]) insert ],

        [ keyPresses (toDoc [has . textClipboard, has . textCopy])
            (keys ^. keysCopy) (str, mempty & State.uSetSystemClipboard ?~ str ) ]

        ]
    where
        insertNewlineKeys = mayShift ModKey.Key'Enter
        insertSpaceKeys = mayShift ModKey.Key'Space
        mayShift k = [noMods k, ModKey.shift k]

        keys = env ^. has
        leftWord = keyPresses (moveWordDoc [has . Dir.left]) (keys ^. keysMoveLeftWord)
        rightWord = keyPresses (moveWordDoc [has . Dir.right]) (keys ^. keysMoveRightWord)
        left =
            E.keyPressesOrRepeat (keys ^. keysDir . StdKeys.keysLeft)
            (moveDoc [has . Dir.left])
        right =
            E.keyPressesOrRepeat (keys ^. keysDir . StdKeys.keysRight)
            (moveDoc [has . Dir.right])
        ( logicalRetreatWord, logicalAdvanceWord
            , logicalRetreat, logicalAdvance) =
            case env ^. has of
            Dir.LeftToRight -> (leftWord , rightWord, left , right)
            Dir.RightToLeft -> (rightWord, leftWord , right, left )
        editDoc = toDoc . (has . MomentuTexts.edit :)
        deleteDoc = editDoc . (has . MomentuTexts.delete :)
        insertDoc = editDoc . (has . MomentuTexts.insert :)
        moveWordDoc = moveDoc . (has . textWord :)
        toDoc = E.toDoc env
        moveDoc =
            toDoc
            . (has . MomentuTexts.navigation :)
            . (has . MomentuTexts.move :)
        splitLines = Text.splitOn "\n"
        linesBefore = reverse (splitLines before)
        linesAfter = splitLines after
        prevLine = linesBefore !! 1
        nextLine = linesAfter !! 1
        curLineBefore = head linesBefore
        curLineAfter = head linesAfter
        cursorX = Text.length curLineBefore
        cursorY = length linesBefore - 1

        eventRes = eventResult myId
        moveAbsolute a = eventRes str . max 0 $ min (Text.length str) a
        moveRelative d = moveAbsolute (cursor + d)
        backDelete n = eventRes (Text.take (cursor-n) str <> Text.drop cursor str) (cursor-n)
        delete n = eventRes (before <> Text.drop n after) cursor
        insert l =
            eventRes str' cursor'
            where
                cursor' = cursor + Text.length l
                str' = mconcat [before, l, after]

        backDeleteWord =
            backDelete . Text.length . tillEndOfWord $ Text.reverse before
        deleteWord = delete . Text.length $ tillEndOfWord after

        backMoveWord =
            moveRelative . negate . Text.length . tillEndOfWord $
            Text.reverse before
        moveWord = moveRelative . Text.length $ tillEndOfWord after

        keyPresses = flip E.keyPresses

        textLength = Text.length str
        lineCount = length $ Text.splitOn "\n" str
        (before, after) = Text.splitAt cursor str

getCursor ::
    (MonadReader env m, State.HasCursor env) =>
    m (Text -> Widget.Id -> Maybe Int)
getCursor =
    State.subId <&> f
    where
        f sub str myId =
            sub myId <&> decodeCursor
            where
                decodeCursor [x] = min (Text.length str) $ Binary.decodeS x
                decodeCursor _ = Text.length str

make ::
    (MonadReader env m, Deps env) =>
    m ( EmptyStrings -> Text -> Widget.Id ->
        TextWidget ((,) Text)
      )
make = makeWithAnimId <&> Lens.mapped . Lens.mapped %~ \f w -> f (Widget.toAnimId w) w

align ::
    (Element.SizedElement a, Has Dir.Layout env) =>
    env -> Modes (WithTextPos View) -> Align.WithTextPos a -> Align.WithTextPos a
align env emptyViews widget =
    widget & Align.tValue %~ Element.padToSize env emptySize 0
    where
        emptySize :: Widget.Size
        emptySize =
            toNonEmpty emptyViews
            <&> (^. Align.tValue . View.vSize)
            <&> Lens.mapped %~ Max & sconcat <&> getMax

makeWithAnimId ::
    (MonadReader env m, Deps env) =>
    m ( EmptyStrings -> Text -> Anim.AnimId -> Widget.Id ->
        TextWidget ((,) Text)
      )
makeWithAnimId =
    do
        get <- getCursor
        env <- Lens.view id
        pure $ \emptyStr str animId myId ->
            do
                let mkEmptyView color = mkView env animId ?? TextView.color .~ color
                let emptyColors = env ^. has . sEmptyStringsColors
                let emptyViews = mkEmptyView <$> emptyColors <*> emptyStr
                case get str myId of
                    Nothing -> makeInternal env unfocused str emptyViews animId myId
                    Just pos -> makeFocused env str emptyStr emptyViews pos animId  myId
                    & align env emptyViews
