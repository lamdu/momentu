{-# OPTIONS -Wno-partial-type-signatures #-}
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, TypeFamilies #-}

module Main where

import           Control.Comonad.Representable.Store
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Data.ByteString.Lens
import qualified Data.Char as Char
import           Data.Has (Has(..))
import           Data.IORef (newIORef)
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (fromMaybe)
import           Data.MRUMemo (memoIO)
import           Data.Property (Property(..), pVal, pSet, MkProperty', mkProperty)
import qualified Data.Property as Property
import           Data.Text (Text)
import           Data.Text.Lens
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.GLFW.Utils (framebufferSize)
import           GUI.Momentu ((/-/), (/|/))
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Animation.Engine as Anim
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import           GUI.Momentu.DefaultEnv (defaultEnv)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Font as Font
import           GUI.Momentu.Glue (hbox)
import qualified GUI.Momentu.Main as M
import qualified GUI.Momentu.Main.Config as M
import           GUI.Momentu.ModKey
import qualified GUI.Momentu.Setup as Setup
import           GUI.Momentu.State (assignCursor, updateCursor)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.DropDownList as DropDownList
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.ListBox as ListBox
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as PropTextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified GUI.Momentu.Window as Window
import           System.Directory (doesFileExist)
import qualified System.Info as SysInfo
import           Text.Show.Pretty (ppShow)

import           Prelude.Compat

data Document = Document
    { _dBgColor :: M.Color
    , _dStyles :: NonEmpty Style
    , _dLines :: [(Text, Items Item)]
    } deriving (Read, Show)

data Style = Style
    { _sDescription :: Text
    , _sColor :: M.Color
    } deriving (Read, Show)

data Items a = Items
    { _iHead :: a
    , _iTail :: [(Spacing, a)]
    } deriving (Read, Show, Functor, Foldable, Traversable)

data Spacing = NoSpace | WithSpace
    deriving (Read, Show)

data Item = Item
    { _iIdentifier :: Maybe Text
    , _iContent :: Text
    , _iStyle :: Int
    } deriving (Read, Show)

Lens.makeLenses ''Document
Lens.makeLenses ''Style
Lens.makeLenses ''Items
Lens.makeLenses ''Item

defaultDoc :: Document
defaultDoc =
    Document
    { _dBgColor = M.Color 0 0 0 0
    , _dStyles = Style "default" (M.Color 1 1 1 1) :| []
    , _dLines =
        [("hello", Items (Item Nothing "hello" 0)
            [(WithSpace, Item Nothing "world" 0)])]
    }

mkIOProp :: a -> IO (MkProperty' IO a)
mkIOProp x = newIORef x <&> Property.fromIORef

mkFileProp :: (Read a, Show a) => FilePath -> a -> IO (MkProperty' IO a)
mkFileProp path def =
    doesFileExist path <&> guard
    >>= Lens._Just (const (readFile path <&> read))
    <&> fromMaybe def
    >>= newIORef <&> Property.fromIORef
    <&> mkProperty . Lens.mapped . pSet <>~ writeFile path . ppShow

setup :: MkProperty' IO Anim.Config -> (Vector2 Double -> M.DefaultEnvWithCursor -> IO (M.Widget IO)) -> IO ()
setup animOptions mk =
    do
        win <- Window.create "text-anim" Window.Maximized
        fontPath <- getDefaultFontPath
        cachedOpenFont <- memoIO (Font.openFont Font.LCDSubPixelDisabled ?? fontPath)
        let cachedOpenFontBySizeFactor = cachedOpenFont . (* 24)
        mainLoop <- M.mainLoopWidget
        env <- cachedOpenFontBySizeFactor 1 <&> defaultEnv SysInfo.os
        opts <-
            M.defaultOptions SysInfo.os env
            <&> M.oConfig . M.cAnim .~ Property.getP animOptions
        M.run mainLoop win M.Handlers
            { M.makeWidget =
                \e ->
                framebufferSize win
                >>= (e &) . Setup.defaultMakeWidget SysInfo.os cachedOpenFontBySizeFactor . const . mk
                <&> Widget.weakerEvents (M.quitEventMap env)
            , M.options = opts
            }
        & M.withGLFW

main :: IO ()
main =
    do
        animOptions <-
            mkIOProp Anim.Config
            { Anim._acTimePeriod = 0.2
            , Anim._acRemainingRatioInPeriod = 0.2
            , Anim._acSpiral = Anim.SpiralAnimConf 0.4 2
            }
        makeWidget animOptions <$> mkFileProp "text-anim.show" defaultDoc <*> mkIOProp 0
            >>= setup animOptions

tryShow :: (Show a, Read a) => a -> Lens.AnIso' a String
tryShow x = Lens.iso show (fromMaybe x . (^? Lens._Show))

makeWidget ::
    _ =>
    MkProperty' IO Anim.Config -> MkProperty' IO Document -> MkProperty' IO Int ->
    Vector2 Double -> env -> IO (M.Widget IO)
makeWidget anim docRef curLineRef winSize env =
    do
        animProp <- anim ^. mkProperty
        docProp <- docRef ^. mkProperty
        curLineProp <- curLineRef ^. mkProperty
        let mkTop = 
                Label.make "Background:"
                /|/ mkColorEdit (Property.composeLens dBgColor docProp) bgEditId
                /|/ Label.make "Anim:"
                /|/ (PropTextEdit.makeWordEdit ?? emptyTexts
                    ?? Property.composeLens (Anim.acTimePeriod . Lens.cloneIso (tryShow 0.11) . packed) animProp
                    ?? Widget.Id ["anim-speed"])
                /|/ Label.make "Tan:"
                /|/ (PropTextEdit.makeWordEdit ?? emptyTexts
                    ?? Property.composeLens (Anim.acSpiral . Anim.sTan . Lens.cloneIso (tryShow 0.1) . packed) animProp
                    ?? Widget.Id ["anim-tan"])
                /|/ Label.make "Thres:"
                /|/ (PropTextEdit.makeWordEdit ?? emptyTexts
                    ?? Property.composeLens (Anim.acSpiral . Anim.sThreshold . Lens.cloneIso (tryShow 0) . packed) animProp
                    ?? Widget.Id ["anim-spiral-thres"])
                /-/ makeStylesEdit (Property.composeLens dStyles docProp)
        let mk =
                do
                    top <- mkTop
                    pure top /-/
                        makeContentEdit (winSize & Lens._2 -~ top ^. Element.size . Lens._2)
                        docProp curLineProp
        (M.backgroundColor ?? docProp ^. pVal . dBgColor) <*> mk
            & assignCursor mempty bgEditId
            & (`runReader` env)
            & pure
    <&> (^. M.tValue)
    where
        bgEditId = Widget.Id ["backgroundEdit"]

makeContentEdit :: _ => Vector2 Double -> Property f Document -> Property f Int -> m (M.TextWidget f)
makeContentEdit size docProp lineProp =
    do
        left <-
            makeLineChoice lineProp (Property.composeLens dLines docProp)
            /-/ (Label.makeFocusable "+" <&> M.tValue %~ M.weakerEvents newLineEventMap)
            <&> (^. M.tValue)
        pure left /|/
            ((Element.padToSize ?? (size & Lens._1 -~ left ^. Element.size . Lens._1) ?? 0.5) <*>
                makeLineEdit (docProp ^. pVal . dStyles) lineContentProp)
    where
        lineContentProp =
            Property.composeLens (dLines . Lens.singular (Lens.ix (lineProp ^. pVal)) . Lens._2) docProp
            & pVal %~ fixClashes
        newLineEventMap =
            E.keyPresses ([M.Key'Space, M.Key'Enter] <&> noMods) (E.Doc ["New line"])
            (mempty <$ Property.pureModify docProp (dLines <>~ [("Untitled", Items (Item Nothing "empty" 0) [])]))

fixClashes :: Items Item -> Items Item
fixClashes items =
    items & Lens.traversed %@~ fixItem
    where
        clashingIdents = items ^.. traverse <&> ident & L.sort & L.group >>= take 1 . drop 1
        fixItem i x
            | ident x `elem` clashingIdents = x & iIdentifier ?~ ('_' : show i) ^. packed
            | otherwise = x

makeLineEdit :: _ => NonEmpty Style -> Property f (Items Item) -> m (M.TextWidget f)
makeLineEdit styles prop =
    do
        addAfterFirst <-
            mkSubIdxId 1 <&>
            \dst x -> dst <$ Property.pureModify prop (iTail %~ (x :))
        hbox <*>
            ((:)
                <$> makeItemEdit styles 0 addAfterFirst (Property.composeLens iHead prop)
                <*> Lens.itraverse (makeSpaceAndItemEdit styles) (propItems (iTail . concatIso . traverse) prop))
    & local (M.animIdPrefix <>~ ["line"])

makeSpaceAndItemEdit :: _ => NonEmpty Style -> Int -> Property f [(Spacing, Item)] -> m (M.TextWidget f)
makeSpaceAndItemEdit styles idx p =
    case p ^? pVal . Lens.ix 0 of
    Nothing -> pure Element.empty
    Just (space, item) ->
        do
            addAfter <-
                mkSubIdxId (idx + 2) <&>
                \dst x -> dst <$ Property.pureModify p (Lens._tail %~ (x :))
            prevId <- mkSubIdxId idx
            let
                (spaceView, delEventMap) =
                    case space of
                    NoSpace ->
                        ( pure Element.empty
                        , if item ^. iContent == ""
                            then
                                E.keysEventMapMovesCursor [noMods M.Key'Backspace] (E.Doc ["delete item"])
                                (prevId <$ Property.pureModify p (drop 1))
                            else mempty
                        )
                    WithSpace ->
                        ( Spacer.stdHSpace
                        , E.keyPress (noMods M.Key'Backspace) (E.Doc ["delete space"])
                            (mempty <$ Property.pureModify p (Lens.ix 0 . Lens._1 .~ NoSpace))
                        )
                setItem x
                    | Lens.has (iContent . Lens.ix 0 . Lens.only ' ') x =
                        Property.pureModify p (Lens.ix 0 . Lens._1 .~ WithSpace)
                    | otherwise = Property.pureModify p (Lens.ix 0 . Lens._2 .~ x)
            spaceView /|/ makeItemEdit styles (idx+1) addAfter (Property item setItem)
                <&> M.tValue %~ M.weakerEvents delEventMap

makeItemEdit :: _ => NonEmpty Style -> Int -> ((Spacing, Item) -> f Widget.Id) -> Property f Item -> m (M.TextWidget f)
makeItemEdit styles idx addItem prop =
    do
        view <-
            TextView.make ?? prop ^. pVal . iContent ?? ["word", show (ident (prop ^. pVal)) ^. packedChars]
            & local setColor
        textId <- mkSubId ["text"]
        baseId <- mkSubId []
        ( TextEdit.make ?? emptyTexts ?? prop ^. pVal . iContent ?? textId
            <&> M.tValue . Widget.updates %~
                ( \(newText, update) ->
                    case newText ^? Lens._Snoc of
                    Just (c, ' ') | c /= "" ->
                        addItem (WithSpace, Item Nothing "" 0) <&> updateCursor
                    Just (xs, x) |
                        Lens.hasn't (Lens.ix 0 . Lens.only ' ') xs &&
                        Lens.anyOf (Lens.from packed . traverse) (mismatch x) xs ->
                        addItem (NoSpace, Item Nothing ([x] ^. packed) 0) <&> updateCursor
                    _ -> update <$ Property.pureModify prop (iContent .~ newText)
                )
            & local setColor
            ) /-/ makeStyleChoice styles (Property.composeLens iStyle prop)
            /-/ ((PropTextEdit.makeWordEdit ?? emptyTexts ?? Property.composeLens (iIdentifier . Lens.non "") prop) <*> mkSubId ["ident"])
            <&> M.tValue . Widget.wSize .~ view ^. Element.size
            <&> M.tValue .  Widget.wState . Widget._StateUnfocused . Widget.uLayers .~
                    view ^. M.tValue . View.vAnimLayers
            & assignCursor baseId textId
    & local (M.animIdPrefix <>~ [show idx ^. packedChars])
    where
        setColor = has . TextView.styleColor .~ styles ^?! Lens.ix (prop ^. pVal . iStyle) . sColor
        mismatch '-' _ = False
        mismatch _ '-' = False
        mismatch x y = cat x /= cat y
        cat :: Char -> Text
        cat x
            | Char.isLetter x = "letter"
            | Char.isDigit x = "digit"
            | x `elem` ("()[]{}" :: String) = Lens._Cons # (x, "")
            | otherwise = "other"

ident :: Item -> Text
ident x = fromMaybe (x ^. iContent) (x ^. iIdentifier)

makeStyleChoice :: _ => NonEmpty Style -> Property.Property f Int -> m (M.TextWidget f)
makeStyleChoice styles prop =
    (DropDownList.make ?? prop)
    <*> Lens.itraverse mk styles
    <*> (DropDownList.defaultConfig ?? "Style")
    <*> mkSubId ["style"]
    where
        mk i s =
            Label.makeFocusable (s ^. sDescription)
            & local (has . TextView.styleColor .~ s ^. sColor)
            <&> (,) i

makeLineChoice :: _ => Property.Property f Int -> Property.Property f [(Text, a)] -> m (M.TextWidget f)
makeLineChoice choice lns =
    (ListBox.make ?? choice ^. pSet)
    <*> Lens.itraverse mk (propItems (traverse . Lens._1) lns)
    ?? ListBox.defaultConfig
    where
        mk i prop =
            do
                orderUp <-
                    if i == 0
                    then pure mempty
                    else
                        mkSubIdxId (i-1) <&>
                        \dst ->
                        E.keysEventMapMovesCursor [shift M.Key'Up] (E.Doc ["Move up"])
                        (dst <$ Property.pureModify lns (swapItems (i-1)))
                orderDown <-
                    if i + 1 == length (lns ^. pVal)
                    then pure mempty
                    else
                        mkSubIdxId (i+1) <&>
                        \dst ->
                        E.keysEventMapMovesCursor [shift M.Key'Down] (E.Doc ["Move down"])
                        (dst <$ Property.pureModify lns (swapItems i))
                (PropTextEdit.makeWordEdit ?? emptyTexts ?? prop) <*> mkSubIdxId i
                    <&> M.tValue %~ M.weakerEvents (orderUp <> orderDown)
            <&> (,) i

swapItems :: Int -> [a] -> [a]
swapItems 0 (x : y : rest) = y : x : rest
swapItems i (x : xs) = x : swapItems (i - 1) xs
swapItems _ [] = []

rgbElems :: Lens.Iso' M.Color [Double]
rgbElems =
    Lens.iso t f
    where
        t (M.Color r g b _a) = [r, g, b]
        f x =
            M.Color r g b 1
            where
                (r:g:b:_) = x <> repeat 0

rgbText :: Lens.Iso' M.Color Text
rgbText = rgbElems . Lens.iso (unwords . map show) ((^.. traverse . Lens._Show) . words) . packed

emptyTexts :: TextEdit.EmptyStrings
emptyTexts = TextEdit.Modes "_" "_"

mkColorEdit ::
    _ =>
    Property f M.Color -> Widget.Id ->
    m (M.TextWidget f)
mkColorEdit prop myId =
    TextEdit.make ?? emptyTexts ?? prop ^. pVal . rgbText ?? myId
    <&> M.tValue . Widget.updates %~
        \(newText, update) -> update <$ (prop ^. pSet) (rgbText # newText)

mkSubId :: _ => M.AnimId -> m Widget.Id
mkSubId x = Element.subAnimId ?? x <&> Widget.Id

mkSubIdxId :: _ => Int -> m Widget.Id
mkSubIdxId x = Element.subAnimId ?? [show x ^. packedChars] <&> Widget.Id

propItems :: Lens.ATraversal' s a -> Property m s -> [Property m a]
propItems t prop =
    Lens.holesOf (Lens.cloneTraversal t) (prop ^. pVal)
    <&> \x -> Property (pos x) ((prop ^. pSet) . (`peek` x))

concatIso :: Lens.Iso [a] [b] [[a]] [[b]]
concatIso = Lens.iso (fmap pure) concat

makeStylesEdit ::
    _ =>
    Property f (NonEmpty Style) ->
    m (M.TextWidget f)
makeStylesEdit prop =
    (hbox <*> Lens.itraverse makeOne (propItems traverse prop))
    /|/
    do
        myId <- mkSubId []
        let addColor =
                myId <> Widget.Id [show (length cols) ^. packedChars] <$
                (prop ^. pSet) (cols <> (Style "untitled" (M.Color 0 0 0 1) :| []))
                where
                    cols = prop ^. pVal
        Label.makeFocusable "+" <&> M.tValue %~
            M.weakerEvents
            (E.keysEventMapMovesCursor ([M.Key'Space, M.Key'Enter] <&> M.noMods)
                (E.Doc ["Edit", "Add color"]) addColor)
    & local (M.animIdPrefix <>~ ["Styles"])
    where
        makeOne idx p =
            ((PropTextEdit.makeWordEdit ?? emptyTexts ?? Property.composeLens sDescription p) <*> mkSubId ["nameEdit"]
                & local (has . TextView.styleColor .~ p ^. pVal . sColor)
            )
            /-/ (mkSubId ["color"] >>= mkColorEdit (Property.composeLens sColor p))
            /|/ Spacer.stdHSpace
            & local (M.animIdPrefix <>~ [show idx ^. packedChars])
