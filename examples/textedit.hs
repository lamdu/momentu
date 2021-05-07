{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Control.Monad.Reader as Reader
import           Data.Has (has)
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import           Data.MRUMemo (memoIO)
import           Data.Text (Text)
import           GUI.Momentu ((/-/))
import qualified GUI.Momentu as M
import           GUI.Momentu.Align (Aligned(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit

import           Prelude.Compat

main :: IO ()
main =
    do
        win <- M.createWindow "TextEdit" M.Maximized
        fontPath <- getDefaultFontPath
        cachedOpenFont <- memoIO (M.openFont M.LCDSubPixelEnabled ?? fontPath)
        font <- cachedOpenFont 24
        mainLoop <- M.mainLoopWidget
        opts <- M.defaultOptions (M.defaultEnv font)
        textRef <- newIORef ("Text", "טקסט")
        M.runMainLoop mainLoop win M.Handlers
            { M.makeWidget = makeWidget cachedOpenFont textRef
            , M.options = opts
            }
        & M.withGLFW

mkTextEdit ::
    TextEdit.Deps env =>
    IORef ioref -> Text -> Text -> Widget.Id -> Lens.ALens' ioref Text ->
    IO (env -> Widget.Widget IO)
mkTextEdit textRef uempty fempty myId alens =
    readIORef textRef
    <&> (^# alens)
    <&> \curText localEnv ->
    TextEdit.make localEnv (TextEdit.Modes uempty fempty)
    curText myId
    & (^. M.tValue)
    & Widget.updates %~
    \(newText, update) ->
        update <$ modifyIORef textRef (alens #~ newText)

makeWidget :: (Float -> IO M.Font) -> IORef (Text, Text) -> M.MainLoopEnv -> IO (M.Widget IO)
makeWidget getFont textRef mainLoopEnv =
    do
        sizeFactor <- M.getZoomFactor (mainLoopEnv ^. M.eZoom)
        font <- getFont (sizeFactor * 20)
        let env =
                M.defaultEnvWithCursor (mainLoopEnv ^. M.eState) font
                & State.cursor %~ assignCursor
        ltrTextEdit <- mkTextEdit textRef "Unfocused empty text" "Focused empty text" ltrId _1
        rtlTextEdit <-
            mkTextEdit textRef "ריק ולא בפוקוס" "ריק ובפוקוס" rtlId _2
            <&> Reader.local (has .~ Dir.RightToLeft)
        env & (Aligned 1 . ltrTextEdit) /-/ (Aligned 1 . rtlTextEdit)
            & (^. Align.value)
            & M.weakerEvents (M.quitEventMap env)
            & pure
    where
        ltrId = Widget.Id ["ltr"]
        rtlId = Widget.Id ["rtl"]
        assignCursor (Widget.Id []) = ltrId
        assignCursor c = c
