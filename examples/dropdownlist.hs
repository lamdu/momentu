{-# LANGUAGE NoImplicitPrelude, DisambiguateRecordFields #-}

module Main (main) where

import           Control.Lens.Operators
import qualified Data.IORef as IORef
import           Data.IORef (IORef)
import qualified Data.Property as Property
import           Data.Text (Text)
import           GUI.Momentu ((/-/))
import qualified GUI.Momentu as M
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import qualified GUI.Momentu.Widgets.DropDownList as DropDownList
import qualified GUI.Momentu.Widgets.Label as Label

import           Prelude.Compat

main :: IO ()
main =
    do
        fontPath <- getDefaultFontPath
        dropDownListRef <- IORef.newIORef "blue"
        M.defaultSetup "DropDownList" fontPath M.defaultSetupOptions (makeWidget dropDownListRef)

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

makeWidget :: IORef Text -> (Float -> IO M.Font) -> M.DefaultEnvWithCursor -> IO (M.Widget IO)
makeWidget dropDownListRef _getFont env =
    do
        prop <- Property.fromIORef dropDownListRef ^. Property.mkProperty
        let dropDownListWidget =
                DropDownList.make env prop (map makeDropDownList colors)
                (DropDownList.defaultConfig env "Color") []
                ^. M.tValue
        let Just color = lookup (Property.value prop) colors
        let box =
                M.unitSquare env
                & M.tint color
                & M.scale 100
        (pure box /-/ pure dropDownListWidget) env
            & M.weakerEvents (M.quitEventMap env)
            & pure
    where
        makeDropDownList (name, _color) = (name, Label.makeFocusable name env)
