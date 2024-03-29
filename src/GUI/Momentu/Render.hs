{-# LANGUAGE CPP #-}

#ifdef darwin_HOST_OS
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
#endif

-- | Render images to windows

module GUI.Momentu.Render
    ( PerfCounters(..)
    , render
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFW.Utils
import           System.TimeIt (timeItT)

import           GUI.Momentu.Prelude

#ifdef darwin_HOST_OS
import qualified Language.C.Inline as C

C.include "<OpenGL/OpenGL.h>"
#endif

data PerfCounters = PerfCounters
    { renderTime :: Double
    , swapBuffersTime :: Double
    }

render :: GLFW.Window -> Draw.Image a -> IO PerfCounters
render win image =
    do

#ifdef darwin_HOST_OS
        [C.exp| void { CGLLockContext(CGLGetCurrentContext()); } |]
#endif

        Vector2 sizeX sizeY <- GLFW.Utils.framebufferSize win
        GL.viewport $=
            (GL.Position 0 0,
             GL.Size (round sizeX) (round sizeY))
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho 0 sizeX sizeY 0 (-1) 1
        (timedRender, ()) <- timeItT (Draw.clearRender image)
        (timedSwapBuffers, ()) <- timeItT (GLFW.swapBuffers win)

#ifdef darwin_HOST_OS
        [C.exp| void { CGLUnlockContext(CGLGetCurrentContext()); } |]
#endif

        pure PerfCounters
            { renderTime = timedRender
            , swapBuffersTime = timedSwapBuffers
            }
