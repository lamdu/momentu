{-# LANGUAGE NoImplicitPrelude, DisambiguateRecordFields #-}

module Main (main) where

import           Control.Lens.Operators
import qualified GUI.Momentu as M
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit

import           Prelude.Compat

main :: IO ()
main =
    do
        fontPath <- getDefaultFontPath
        M.defaultSetup "Search-menu" fontPath M.defaultSetupOptions makeWidget

makeWidget :: (Float -> IO M.Font) -> M.DefaultEnvWithCursor -> IO (M.Widget IO)
makeWidget _getFont env =
    SearchMenu.make (const makeSeachTerm) makeResults Element.empty menuId env SearchMenu.AnyPlace
    ^. M.tValue
    & pure
    where
        menuId = Widget.Id []
        makeResults (SearchMenu.ResultsContext _searchTerm _resultIdPrefix) =
            pure (SearchMenu.FullList [])
        emptyStrings =
            TextEdit.Modes
            { TextEdit._unfocused = "?"
            , TextEdit._focused = "_"
            }
        makeSeachTerm =
            SearchMenu.basicSearchTermEdit ["search-term"] menuId (const (pure True)) emptyStrings
