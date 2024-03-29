-- | Low-level event-loop

{-# LANGUAGE CPP #-}

#ifdef darwin_HOST_OS
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
#endif

module GUI.Momentu.Main.Events.Loop
    ( Event(..)
    , Next(..)
    , EventLoopDisallowedWhenMasked(..)
    , Handlers(..)
    , eventLoop, wakeUp
    ) where

import qualified Control.Exception as E
import           Data.Typeable (Typeable)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Main.Events
import           GUI.Momentu.ModKey (fromGLFWModifiers)
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFW.Utils

import           GUI.Momentu.Prelude

#ifdef darwin_HOST_OS
import qualified Language.C.Inline.ObjC as C

C.context C.objcCtx

C.include "<Cocoa/Cocoa.h>"
#endif

-- | The output of the event handler back to the event-loop.
data Next
    = NextWait
    -- ^ idle wait for the next event, or an explicit wake-up
    | NextPoll
    -- ^ poll for the next event and immediately (up to sync-to-vblank) continue
    | NextQuit
    -- ^ The event-loop should quit.
    deriving (Show, Eq, Ord)

data EventLoopDisallowedWhenMasked = EventLoopDisallowedWhenMasked
    deriving stock (Generic, Show, Typeable)
    deriving anyclass E.Exception

mouseButtonEvent ::
    GLFW.Window -> (Event -> IO a) -> GLFW.MouseButton -> GLFW.MouseButtonState ->
    GLFW.ModifierKeys -> IO a
mouseButtonEvent win handler button buttonState modKeys =
    do
        fbSize <- GLFW.Utils.framebufferSize win
        winSize <- GLFW.Utils.windowSize win
        p <- GLFW.getCursorPos win <&> uncurry Vector2
        EventMouseButton MouseButtonEvent
            { mbButton = button
            , mbButtonState = buttonState
            , mbModKeys = fromGLFWModifiers modKeys
            , mbPosition = p * fbSize / winSize
            , mbPositionInWindowCoords = p
            } & handler

keyEvent :: (Event -> IO Bool) -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO Bool
keyEvent handler key scanCode keyState modKeys =
    EventKey KeyEvent
    { keKey = key
    , keScanCode = scanCode
    , keState = keyState
    , keModKeys = fromGLFWModifiers modKeys
    } & handler

framebufferSizeEvent :: (Event -> a) -> Int -> Int -> a
framebufferSizeEvent handler w h =
    Vector2 w h & EventFramebufferSize & handler

charEvent :: Monoid a => (Event -> a) -> Char -> a
charEvent handler char
    -- Range for "key" characters (keys for left key, right key, etc.)
    | char < '\57344' || '\63743' < char = EventChar char & handler
    | otherwise = mempty

validateMasksingState :: IO ()
validateMasksingState =
    do
        maskingState <- E.getMaskingState
        when (maskingState /= E.Unmasked) $ E.throwIO EventLoopDisallowedWhenMasked

-- | Exit current iteration, when it is waiting on an event
wakeUp :: IO ()
wakeUp = GLFW.postEmptyEvent

data Handlers = Handlers
    { eventHandler :: Event -> IO Bool -- ^ returns whether event was handled
    , iteration :: IO Next -- ^ How to wait for next iteration
    }

eventLoop :: GLFW.Window -> Handlers -> IO ()
eventLoop win handlers =
    do
        validateMasksingState
        let setCallback f cb = f win $ Just $ const cb
        setCallback GLFW.setCharCallback (charEvent vhandler)
        setCallback GLFW.setKeyCallback (keyEvent handler)
        setCallback GLFW.setMouseButtonCallback (mouseButtonEvent win vhandler)
        setCallback GLFW.setDropCallback (vhandler . EventDropPaths)
        setCallback GLFW.setWindowRefreshCallback $ vhandler EventWindowRefresh
        setCallback GLFW.setFramebufferSizeCallback $ framebufferSizeEvent vhandler
        setCallback GLFW.setWindowCloseCallback $ vhandler EventWindowClose

#ifdef darwin_HOST_OS
        cocoaWin <- GLFW.getCocoaWindow win
        [C.block| void {
            // Enable animations during resize (when user set up a refresh callback).
            // Based on https://developer.apple.com/library/archive/qa/qa1385/_index.html
            [[NSRunLoop currentRunLoop]
                addTimer:
                    [NSTimer
                        timerWithTimeInterval:0.001 // ms
                        target:[(id) $(void* cocoaWin) contentView]
                        selector:@selector(setNeedsDisplay:)
                        userInfo:@YES
                        repeats:YES]
                forMode:NSEventTrackingRunLoopMode];
        } |]
#endif

        vhandler EventWindowRefresh

        let loop =
                iteration handlers
                >>= \case
                NextWait -> GLFW.waitEvents *> loop
                NextPoll -> GLFW.pollEvents *> loop
                NextQuit -> pure ()
        loop
    where
        vhandler = void . handler
        handler = eventHandler handlers
