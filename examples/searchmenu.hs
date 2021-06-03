{-# LANGUAGE NoImplicitPrelude, DisambiguateRecordFields #-}

module Main (main) where

import           Control.Lens.Operators
import           GUI.Momentu ((/-/))
import qualified GUI.Momentu as M
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu

import           Prelude.Compat

main :: IO ()
main =
    do
        fontPath <- getDefaultFontPath
        M.defaultSetup "Search-menu" fontPath M.defaultSetupOptions makeWidget

makeWidget :: (Float -> IO M.Font) -> M.DefaultEnvWithCursor -> IO (M.Widget IO)
makeWidget _getFont env =
    env
    &
    ( (SearchMenu.make makeSearchTerm makeResults Element.empty menuId ?? SearchMenu.AnyPlace)
        /-/
        Label.makeFocusable "Some Text 1"
        /-/
        Label.makeFocusable "Some Text 2"
        & State.assignCursor mempty menuId
    )
    & (^. M.tValue)
    & pure
    where
        menuId = Widget.Id ["Menu"]
        makeResults (SearchMenu.ResultsContext _searchTerm _resultIdPrefix) =
            pure SearchMenu.OptionList
            { SearchMenu._olIsTruncated = True
            , SearchMenu._olOptions = []
            }
        makeSearchTerm = SearchMenu.searchTermEdit menuId (const (pure True))
