{-# LANGUAGE TemplateHaskell, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, FlexibleInstances #-}
module GUI.Momentu.Main
    ( Config(..)
    , Env(..), eWindowSize, eZoom, eState
    , HasMainLoopEnv
    , DebugOptions(..), defaultDebugOptions
    , PerfCounters(..)
    , Options(..), oConfig, oStateStorage, oDebug, oGetTexts
    , defaultOptions
    , quitEventMap
    , MainLoop(..), Handlers(..), mainLoopWidget
    , Texts(..), textQuit, textJumpToSource, textDebug
    , englishTexts
    , AllTexts(..), makeAllTexts
    , Keys(..), keysQuit
    , OSString, stdKeys
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2)
import           GHC.Stack (CallStack, SrcLoc, prettySrcLoc, getCallStack)
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.I18N as Texts
import           GUI.Momentu.Main.Animation (PerfCounters(..), MainLoop(..))
import qualified GUI.Momentu.Main.Animation as MainAnim
import           GUI.Momentu.Main.Config (Config(..))
import qualified GUI.Momentu.Main.Config as MainConfig
import           GUI.Momentu.Main.Events (MouseButtonEvent(..))
import qualified GUI.Momentu.Main.Events as Main.Events
import           GUI.Momentu.MetaKey (OSString, cmd)
import           GUI.Momentu.ModKey (ModKey)
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Rect (Rect)
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.State (GUIState(..))
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget, R)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Cursor as Cursor
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom
import           Graphics.UI.GLFW (MouseButton(..), MouseButtonState(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFW.Utils

import           GUI.Momentu.Prelude

data Texts a = Texts
    { _textQuit :: a
    , _textJumpToSource :: a
    , _textDebug :: a
    } deriving Eq

Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_text" ''Texts

newtype Keys key = Keys
    { _keysQuit :: [key]
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

Lens.makeLenses ''Keys
JsonTH.derivePrefixed "_keys" ''Keys

stdKeys :: OSString -> Keys ModKey
stdKeys os =
    Keys
    { _keysQuit = [cmd os ModKey.Key'Q]
    }

englishTexts :: Texts Text
englishTexts = Texts
    { _textQuit = "Quit"
    , _textJumpToSource = "Jump to source"
    , _textDebug = "Debug"
    }

data AllTexts = AllTexts
    { _mainTexts :: Texts Text
    , _commonTexts :: Texts.Texts Text
    , _zoomTexts :: Zoom.Texts Text
    }
Lens.makeLenses ''AllTexts

instance Has (Texts Text) AllTexts where has = mainTexts
instance Has (Texts.Texts Text) AllTexts where has = commonTexts
instance Has (Zoom.Texts Text) AllTexts where has = zoomTexts

makeAllTexts ::
    (Has (Texts Text) env, Has (Zoom.Texts Text) env, Has (Texts.Texts Text) env) =>
    env -> AllTexts
makeAllTexts env = AllTexts (env ^. has) (env ^. has) (env ^. has)

data DebugOptions = DebugOptions
    { fpsFont :: Zoom -> IO (Maybe Font)
    , virtualCursorColor :: IO (Maybe Draw.Color)
    , reportPerfCounters :: PerfCounters -> IO ()
    , jumpToSourceKeys :: IO [ModKey]
    , jumpToSource :: SrcLoc -> IO ()
    , postProcessEvent :: Main.Events.Event -> IO ()
    }

iorefStateStorage :: Widget.Id -> IO (MkProperty' IO GUIState)
iorefStateStorage initialCursor =
    newIORef (GUIState initialCursor mempty) <&> Property.fromIORef

data Options = Options
    { _oConfig :: Config
    , _oStateStorage :: MkProperty' IO GUIState
    , _oDebug :: DebugOptions
    , _oGetTexts :: IO AllTexts
    }

Lens.makeLenses ''Options

data Handlers = Handlers
    { makeWidget :: Env -> IO (Widget IO)
    , options :: Options
    }

defaultDebugOptions :: DebugOptions
defaultDebugOptions =
    DebugOptions
    { fpsFont = const (pure Nothing)
    , virtualCursorColor = pure Nothing
    , reportPerfCounters = const (pure ())
    , jumpToSourceKeys = pure []
    , jumpToSource = \_ -> pure ()
    , postProcessEvent = \_ -> pure ()
    }

defaultOptions ::
    ( Has (Texts Text) env, Has (Zoom.Texts Text) env
    , Element.HasAnimIdPrefix env, Has EventMapHelp.Config env
    , EventMapHelp.HasStyle env, EventMapHelp.HasTexts env
    ) =>
    OSString -> env -> IO Options
defaultOptions os env =
    do
        helpProp <- newIORef EventMapHelp.HelpNotShown <&> Property.fromIORef
        -- Note that not every app is necessarily interactive and even uses a cursor,
        -- so an empty value might be fitting.
        stateStorage <- iorefStateStorage (Widget.Id [])
        pure Options
            { _oConfig = Config
                { _cAnim =
                    pure MainAnim.Config
                    { MainAnim._acTimePeriod = 0.11
                    , MainAnim._acRemainingRatioInPeriod = 0.2
                    , MainAnim._acSpiral = MainAnim.SpiralAnimConf 0 0
                    }
                , _cCursor =
                    \_zoom -> pure Cursor.Config
                    { Cursor.cursorColor = Draw.Color 0.5 0.5 1 0.5
                    , Cursor.decay = Nothing
                    }
                , _cZoom = pure (Zoom.defaultConfig os)
                , _cPostProcess =
                    \_zoom size widget ->
                    helpProp ^. Property.mkProperty <&>
                    \prop -> EventMapHelp.toggledHelpAdder env prop size widget
                , _cInvalidCursorOverlayColor = pure (Draw.Color 1.0 0 0 0.3)
                }
            , _oStateStorage = stateStorage
            , _oDebug = defaultDebugOptions
            , _oGetTexts = makeAllTexts env & pure
            }

quitEventMap ::
    (MonadReader env m, Has (Texts Text) env, Has (Keys ModKey) env) => m (EventMap (f a))
quitEventMap =
    Lens.view id <&> \env ->
    let doc = E.Doc [env ^. has . textQuit]
    in  E.keyPresses (env ^. has . keysQuit) doc (error "Quit")

mkJumpToSourceEventMap ::
    Functor f => Texts Text -> DebugOptions -> f () -> IO (EventMap (f State.Update))
mkJumpToSourceEventMap txt debug act =
    jumpToSourceKeys debug
    <&> \keys ->
    E.keysEventMap keys
    (E.toDoc txt [textDebug, textJumpToSource]) act

data Env = Env
    { _eZoom :: Zoom
    , _eWindowSize :: Widget.Size
    , _eState :: GUIState
    }
Lens.makeLenses ''Env
instance State.HasCursor Env
instance Has GUIState Env where has = eState

type HasMainLoopEnv env = (State.HasCursor env, Has Env env)

jumpToTopOfCallStack :: DebugOptions -> CallStack -> IO ()
jumpToTopOfCallStack debug callStack =
    case getCallStack callStack of
    [] -> putStrLn "call stack empty, can't jump to it"
    ((_func, topFrame):_) ->
        do
            putStrLn $ "Jumping to: " <> prettySrcLoc topFrame
            jumpToSource debug topFrame

data LookupMode = ApplyEvent | JumpToSource

handleEvent ::
    Monoid a =>
    DebugOptions -> IORef LookupMode -> IO (Maybe E.Clipboard) ->
    IORef (Maybe State.VirtualCursor) ->
    Maybe (Vector2 R -> Widget.EnterResult a) ->
    Maybe (Rect, State.VirtualCursor -> EventMap a) -> Main.Events.Event ->
    IO (Maybe a)
handleEvent debug lookupModeRef getClipboard virtCursorRef mEnter mFocus event =
    postProcessEvent debug event >>
    case event of
    Main.Events.EventKey key -> E.EventKey key & doLookup
    Main.Events.EventChar c -> E.EventChar c & doLookup
    Main.Events.EventDropPaths paths -> E.EventDropPaths paths & doLookup
    Main.Events.EventMouseButton buttonEvent ->
        case (buttonEvent, mEnter) of
        ( MouseButtonEvent MouseButton'1
            MouseButtonState'Released _ mousePosF _
            , Just enter
            ) -> enter mousePosF ^. Widget.enterResultEvent & Just & pure
        _ -> pure Nothing
    Main.Events.EventWindowClose -> fail "Quit"
    Main.Events.EventWindowRefresh -> refresh
    Main.Events.EventFramebufferSize _size -> refresh
    where
        refresh =
            -- Returning a "Just" is a successful lookup - so
            -- schedules a refresh
            pure (Just mempty)
        doLookup =
            case mFocus of
            Just focus ->
                lookupEvent debug lookupModeRef getClipboard virtCursorRef focus
            Nothing -> const (pure Nothing)

lookupEvent ::
    DebugOptions -> IORef LookupMode -> IO (Maybe E.Clipboard) ->
    IORef (Maybe State.VirtualCursor) ->
    (Rect, State.VirtualCursor -> EventMap a) -> E.Event ->
    IO (Maybe a)
lookupEvent debug lookupModeRef getClipboard virtCursorRef (focalArea, mkEventMap) event =
    do
        virtCursorState <- readIORef virtCursorRef
        virtCursor <-
            case virtCursorState of
            Just x -> pure x
            Nothing ->
                res <$ writeIORef virtCursorRef (Just res)
                where
                    res = State.VirtualCursor focalArea
        mDocHandler <- E.lookup getClipboard event (mkEventMap virtCursor)
        case mDocHandler of
            Nothing -> pure Nothing
            Just docHandler ->
                do
                    lookupMode <- readIORef lookupModeRef
                    writeIORef lookupModeRef ApplyEvent
                    case lookupMode of
                        ApplyEvent -> docHandler ^. E.dhHandler & Just & pure
                        JumpToSource ->
                            docHandler ^. E.dhFileLocation
                            & jumpToTopOfCallStack debug
                            & (Nothing <$)

virtualCursorImage :: Maybe State.VirtualCursor -> DebugOptions -> IO Anim.Frame
virtualCursorImage Nothing _ = pure mempty
virtualCursorImage (Just (State.VirtualCursor r)) debug =
    virtualCursorColor debug
    <&> \case
    Nothing -> mempty
    Just color ->
        Anim.coloredRectangle ["debug-virtual-cursor"] color
        & Anim.scale (r ^. Rect.size) & Anim.translate (r ^. Rect.topLeft)

wrapMakeWidget ::
    Zoom -> Options -> IORef LookupMode ->
    (Env -> IO (Widget IO)) ->
    Widget.Size -> IO (Widget IO)
wrapMakeWidget zoom options lookupModeRef mkWidgetUnmemod size =
    do
        s <- Property.getP (options ^. oStateStorage)
        let env = Env
                { _eZoom = zoom
                , _eWindowSize = size
                , _eState = s
                }
        txt <- options ^. oGetTexts
        zoomEventMap <-
            options ^. oConfig . MainConfig.cZoom
            <&> Zoom.eventMap (env ^. eZoom) txt
        jumpToSourceEventMap <-
            writeIORef lookupModeRef JumpToSource
            & mkJumpToSourceEventMap (txt ^. has) (options ^. oDebug)
        let moreEvents = zoomEventMap <> jumpToSourceEventMap
        w <- mkWidgetUnmemod env
        ( if Widget.isFocused w
            then
                pure w
            else
                do
                    putStrLn ("Invalid cursor: " <> show (env ^. State.cursor))
                    showInvalidCursor <*>
                        mkWidgetUnmemod (env & State.cursor .~ mempty)
                        >>= assertFocused
            )
            <&> Widget.eventMapMaker . Lens.mapped %~ (moreEvents <>)
            >>= (options ^. oConfig . MainConfig.cPostProcess) zoom (env ^. eWindowSize)
    where
        assertFocused w
            | Widget.isFocused w = pure w
            | otherwise = fail "Creating widget on the empty cursor failed"
        bgColorAnimId :: AnimId
        bgColorAnimId = ["invalid-cursor-background"]
        showInvalidCursor :: IO (Widget IO -> Widget IO)
        showInvalidCursor =
            options ^. oConfig . MainConfig.cInvalidCursorOverlayColor <&>
            \color ->
            Element.setLayeredImage . Element.layers <. Lens.reversed . Lens.ix 0 %@~
            (<>) . (`Anim.scale` Anim.coloredRectangle bgColorAnimId color)

runInner ::
    IORef (IO ()) -> (GLFW.Window -> MainAnim.Handlers -> IO b) ->
    GLFW.Window -> Handlers -> IO b
runInner refreshAction run win handlers =
    do
        let getClipboard = GLFW.getClipboardString win <&> fmap Text.pack
        let opts = options handlers
        zoom <- Zoom.make win
        lookupModeRef <- newIORef ApplyEvent
        virtCursorRef <- newIORef Nothing
        let mkW =
                wrapMakeWidget zoom opts lookupModeRef
                (makeWidget handlers)
                & memoIO
        mkWidgetRef <- mkW >>= newIORef
        let newWidget = mkW >>= writeIORef mkWidgetRef
        let renderWidget size =
                do
                    virtCursor <- readIORef virtCursorRef
                    vcursorimg <- virtualCursorImage virtCursor (opts ^. oDebug)
                    Cursor.render
                        <$> (readIORef mkWidgetRef >>= (size &))
                        <&> _1 . Lens.mapped %~ (vcursorimg <>)
        -- Tie the knot here: now we know how to wake up, so put it in the IORef
        writeIORef refreshAction newWidget
        run win $
            MainAnim.Handlers
            { MainAnim.reportPerfCounters = reportPerfCounters (opts ^. oDebug)
            , MainAnim.getConfig = opts ^. oConfig . MainConfig.cAnim
            , MainAnim.getFPSFont = fpsFont (opts ^. oDebug) zoom
            , MainAnim.eventHandler = \event ->
                do
                    size <- GLFW.Utils.framebufferSize win
                    (_, mEnter, mFocus) <- renderWidget size
                    mWidgetRes <-
                        handleEvent (opts ^. oDebug) lookupModeRef getClipboard
                        virtCursorRef mEnter mFocus event
                    mRes <- sequenceA mWidgetRes
                    case mRes of
                        Nothing -> pure ()
                        Just res ->
                            do
                                Property.modP (opts ^. oStateStorage) (State.update res)
                                writeIORef virtCursorRef (res ^. State.uVirtualCursor . Lens._Wrapped)
                                res ^.. State.uSetSystemClipboard . Lens._Just &
                                    traverse_ (GLFW.setClipboardString win . Text.unpack)
                                newWidget
                    pure (Lens.has Lens._Just mRes)
            , MainAnim.makeFrame =
                do
                    size <- GLFW.Utils.framebufferSize win
                    (renderWidget size <&> (^. _1))
                        <*> (opts ^. oConfig . MainConfig.cCursor) zoom
            }

mainLoopWidget :: IO (MainLoop Handlers)
mainLoopWidget =
    do
        refreshAction <- newIORef (fail "wakeUp called before run")
        MainAnim.mainLoop <&>
            \mainLoop ->
            mainLoop
            { run = runInner refreshAction (run mainLoop)
            , wakeUp =
                do
                    readIORef refreshAction & join
                    wakeUp mainLoop
            }
