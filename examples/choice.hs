{-# LANGUAGE NoImplicitPrelude, DisambiguateRecordFields #-}

module Main (main) where

import           Control.Lens.Operators
import qualified Data.IORef as IORef
import           Data.IORef (IORef)
import           Data.MRUMemo (memoIO)
import qualified Data.Property as Property
import           Data.Text (Text)
import           GUI.Momentu ((/-/))
import qualified GUI.Momentu as M
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Label as Label

import           Prelude.Compat

main :: IO ()
main =
    do
        win <- M.createWindow "Hello World" M.Maximized
        fontPath <- getDefaultFontPath
        getFont <- memoIO (M.openFont M.LCDSubPixelEnabled ?? fontPath)
        helpFont <- getFont 72
        mainLoop <- M.mainLoopWidget
        opts <- M.defaultOptions (M.defaultEnv helpFont)
        choiceRef <- IORef.newIORef "blue"
        let handlers =
                M.Handlers
                { M.makeWidget = makeWidget choiceRef getFont
                , M.options = opts
                }
        M.runMainLoop mainLoop win handlers
    & M.withGLFW

colors :: [(Text, M.Color)]
colors =
    [ ("black" , M.Color 0 0 0 1)
    , ("blue"  , M.Color 0 0 1 1)
    , ("green" , M.Color 0 1 0 1)
    , ("teal"  , M.Color 0 1 1 1)
    , ("red"   , M.Color 1 0 0 1)
    , ("purple", M.Color 1 0 1 1)
    , ("brown" , M.Color 0.8 0.5 0 1)
    , ("grey"  , M.Color 0.5 0.5 0.5 1)
    ]

makeWidget ::
    IORef Text -> (Float -> IO M.Font) -> M.MainLoopEnv ->
    IO (M.Widget IO)
makeWidget choiceRef getFont mainEnv =
    do
        sizeFactor <- M.getZoomFactor (mainEnv ^. M.eZoom)
        font <- getFont (sizeFactor * 20)
        let env = M.defaultEnvWithCursor (mainEnv ^. M.eState) font
        let makeChoice (name, _color) =
                (name, Label.makeFocusable name env)
        prop <- Property.fromIORef choiceRef ^. Property.mkProperty
        let choiceWidget =
                Choice.make env prop (map makeChoice colors)
                (Choice.defaultConfig env "Color") (M.WidgetId [])
                ^. M.tValue
        let Just color = lookup (Property.value prop) colors
        let box =
                M.unitSquare env
                & M.tint color
                & M.scale 100
        (pure box /-/ pure choiceWidget) env
            & M.weakerEvents (M.quitEventMap env)
            & pure
