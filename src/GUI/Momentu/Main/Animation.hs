module GUI.Momentu.Main.Animation
    ( MainLoop(..)
    , mainLoop
    , Anim.Config(..)
    , Anim.SpiralAnimConf(..)
    , Handlers(..)
    , PerfCounters(..)
    ) where

import           Control.Concurrent.Extended (rtsSupportsBoundThreads, forwardSynchronuousExceptions, withForkedOS)
import           Control.Concurrent.STM (STM)
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
    , eventHandler :: Event -> IO Bool
        -- ^ Returns whether the event was handled, meaning two things:
        -- 1. A new frame will be generated
        -- 2. Do not pass event to further processing (e.g: input managers)
    }

eventThreadLoop ::
    IORef (Maybe UTCTime) ->
    (Maybe Anim.Dest -> IO ()) -> GLFW.Window -> Handlers -> IO ()
eventThreadLoop lastTimestampRef sendNewFrame win handlers =
    EventLoop.eventLoop win EventLoop.Handlers
    { EventLoop.eventHandler = updateTimestamp . eventHandler handlers
    , EventLoop.iteration =
        do
            lastTimestamp <- readIORef lastTimestampRef
            writeIORef lastTimestampRef Nothing
            traverse_ sendStampedFrame lastTimestamp
            pure EventLoop.NextWait
    }
    where
        updateTimestamp act =
            do
                preTimestamp <- getCurrentTime
                didAnything <- act
                when didAnything
                    (writeIORef lastTimestampRef (Just preTimestamp))
                pure didAnything
        sendStampedFrame timestamp =
            makeFrame handlers <&> Anim.Dest timestamp <&> Just
            >>= sendNewFrame

animThreadLoop :: STM (Maybe Anim.Dest) -> Handlers -> GLFW.Window -> IO ()
animThreadLoop recvNewFrame handlers win =
    do
        GLFW.makeContextCurrent (Just win)
        GLFW.swapInterval 1
        makeFrame handlers >>= Anim.initialState >>= loop
    where
        waitNewFrame = STM.atomically $ recvNewFrame >>= maybe STM.retry pure
        pollNewFrame = STM.atomically recvNewFrame
        loop animState =
            do
                Anim.currentFrame animState & Anim.draw & render win >>= reportPerfCounters handlers
                frameReq <- if Anim.isAnimating animState then pollNewFrame else waitNewFrame <&> Just
                animConfig <- getConfig handlers
                Anim.clockedAdvanceAnimation animConfig frameReq animState
                    <&> fromMaybe animState . (^? Anim._NewState)
                    >>= loop

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
    , run = \win handlers ->
        do
            unless rtsSupportsBoundThreads (error "mainLoop requires threaded runtime")
            GLFW.makeContextCurrent Nothing
            newFrameReq <- STM.newTVarIO Nothing
            let recvFrameReq = STM.swapTVar newFrameReq Nothing
            let sendFrameReq = STM.atomically . STM.writeTVar newFrameReq
            animThread <-
                animThreadLoop recvFrameReq handlers win
                & forwardSynchronuousExceptions
            withForkedOS
                (animThread `onException` EventLoop.wakeUp)
                (eventThreadLoop lastTimestampRef sendFrameReq win handlers)
    }
