module GUI.Momentu.Main.Animation
    ( MainLoop(..)
    , mainLoop
    , Anim.Config(..)
    , Anim.SpiralAnimConf(..)
    , Handlers(..)
    , PerfCounters(..)
    ) where

import           Control.Concurrent.Extended (rtsSupportsBoundThreads, forwardSynchronuousExceptions, withForkedOS)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (onException)
import           Data.IORef
import           Data.Time.Clock (UTCTime, getCurrentTime)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Animation.Engine as Anim
import           GUI.Momentu.Font (Font)
import           GUI.Momentu.Main.Events (Event)
import qualified GUI.Momentu.Main.Events.Loop as EventLoop
import           GUI.Momentu.Render (render, PerfCounters(..))
import qualified Graphics.UI.GLFW as GLFW

import           GUI.Momentu.Prelude

data Handlers = Handlers
    { reportPerfCounters :: PerfCounters -> IO ()
    , getFPSFont :: IO (Maybe Font)
    , getConfig :: IO Anim.Config
    , makeFrame :: IO Anim.Frame
    , eventHandler :: Event -> IO (Maybe (IO ()))
        -- ^ Returns whether the event was handled, meaning two things:
        -- 1. A new frame will be generated
        -- 2. Do not pass event to further processing (e.g: input managers)
    }

eventAnimLoop ::
    (IO () -> IO ()) -> IORef (Maybe UTCTime) -> GLFW.Window -> Handlers -> IO ()
eventAnimLoop queueAction lastTimestampRef win handlers =
    do
        GLFW.swapInterval 1
        animStateRef <- makeFrame handlers >>= Anim.initialState >>= newIORef
        let iteration frameReq =
                do
                    writeIORef lastTimestampRef Nothing
                    prevAnimState <- readIORef animStateRef
                    animConfig <- getConfig handlers
                    animRes <- readIORef animStateRef >>= Anim.clockedAdvanceAnimation animConfig frameReq
                    let animState = fromMaybe prevAnimState (animRes ^? Anim._NewState)
                    writeIORef animStateRef animState
                    Anim.currentFrame animState & Anim.draw & render win >>= reportPerfCounters handlers
                    case animRes of
                        Anim.AnimationComplete -> EventLoop.NextWait
                        _ -> EventLoop.NextPoll
                        & pure
        let mkFrameReq timestamp = makeFrame handlers <&> Anim.Dest timestamp
        fbSizeRef <- GLFW.getFramebufferSize win >>= newIORef
        EventLoop.eventLoop win EventLoop.Handlers
            { EventLoop.eventHandler =
                \case
                EventLoop.EventWindowRefresh ->
                    do
                        prevFbSize <- readIORef fbSizeRef
                        curFbSize <- GLFW.getFramebufferSize win
                        if curFbSize == prevFbSize
                            then iteration Nothing & void
                            else
                                do
                                    writeIORef fbSizeRef curFbSize
                                    getCurrentTime >>= mkFrameReq <&> Just >>= iteration & void
                        pure True
                EventLoop.EventFramebufferSize{} -> pure False
                event ->
                    do
                        preTimestamp <- getCurrentTime
                        mEvent <- eventHandler handlers event
                        case mEvent of
                            Nothing -> pure False
                            Just eventAction ->
                                do
                                    do
                                        eventAction
                                        writeIORef lastTimestampRef (Just preTimestamp)
                                        EventLoop.wakeUp
                                        & queueAction
                                    pure True
            , EventLoop.iteration =
                readIORef lastTimestampRef >>= traverse mkFrameReq >>= iteration
            }

data MainLoop handlers = MainLoop
    { wakeUp :: IO ()
    , run :: GLFW.Window -> handlers -> IO ()
    }

mainLoop :: IO (MainLoop Handlers)
mainLoop =
    newIORef Nothing <&>
    \lastTimestampRef ->
    MainLoop
    { wakeUp =
        do
            getCurrentTime <&> Just >>= writeIORef lastTimestampRef
            EventLoop.wakeUp
    , run =
        \win handlers ->
        do
            unless rtsSupportsBoundThreads (error "mainLoop requires threaded runtime")
            eventsQueue <- STM.newTQueueIO
            eventWorkerThread <- STM.readTQueue eventsQueue & STM.atomically & join & forever & forwardSynchronuousExceptions
            withForkedOS
                (eventWorkerThread `onException` EventLoop.wakeUp)
                (eventAnimLoop (STM.atomically . STM.writeTQueue eventsQueue) lastTimestampRef win handlers)
    }
