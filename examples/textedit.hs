{-# OPTIONS -Wno-partial-type-signatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Has (has)
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import           Data.Text (Text)
import           GUI.Momentu ((/-/))
import qualified GUI.Momentu as M
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit

import           Prelude.Compat

main :: IO ()
main =
    do
        fontPath <- getDefaultFontPath
        textRef <- newIORef ("Text", "טקסט")
        M.defaultSetup "TextEdit" fontPath M.defaultSetupOptions (makeWidget textRef)

makeWidget :: _ => IORef s -> p -> env -> IO (M.Widget IO)
makeWidget textRef _getFont rawEnv =
    do
        ltrTextEdit <- mkTextEdit textRef "Unfocused empty text" "Focused empty text" ltrId _1
        rtlTextEdit <-
            mkTextEdit textRef "ריק ולא בפוקוס" "ריק ובפוקוס" rtlId _2
            <&> (. (has .~ Dir.RightToLeft))
        env & (M.Aligned 1 . ltrTextEdit) /-/ (M.Aligned 1 . rtlTextEdit)
            & (^. M.value)
            & M.weakerEvents (M.quitEventMap env)
            & pure
    where
        env = rawEnv & M.cursor %~ assignCursor
        ltrId = ["ltr"]
        rtlId = ["rtl"]
        assignCursor [] = ltrId
        assignCursor c = c

mkTextEdit ::
    TextEdit.Deps env =>
    IORef ioref -> Text -> Text -> M.ElemId -> Lens.ALens' ioref Text ->
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
