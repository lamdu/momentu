{-# LANGUAGE NoImplicitPrelude, DisambiguateRecordFields #-}

module Main (main) where

import           Control.Lens.Operators
import           Data.IORef
import           Data.Text (Text, isInfixOf)
import qualified Data.Text.Encoding as Text
import           GUI.Momentu ((/-/))
import qualified GUI.Momentu as M
import           GUI.Momentu.DataFiles (getDefaultFontPath)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Prelude.Compat

main :: IO ()
main =
    do
        fontPath <- getDefaultFontPath
        textRef <- newIORef "<unselected>"
        M.defaultSetup "Search-menu" fontPath M.defaultSetupOptions (makeWidget textRef)

fewOpts :: [Text]
fewOpts = ["1", "2"]

manyOpts :: [Text]
manyOpts = ["Momentu", "is", "an", "immediate", "GUI", "library"]

filteredOpts :: [Text] -> Text -> [Text]
filteredOpts opts searchTerm = filter (searchTerm `isInfixOf`) opts

makeWidget :: IORef Text -> (Float -> IO M.Font) -> M.DefaultEnvWithCursor -> IO (M.Widget IO)
makeWidget textRef _getFont env =
    do
        text <- readIORef textRef
        env
            &
            ( menu manyOpts (menuId "many")
            /-/ menu fewOpts (menuId "few")
            /-/ menu [] (menuId "none")
            /-/ makeResultWidget text
            & State.assignCursor mempty (menuId "many")
            )
            & (^. M.tValue)
            & pure
    where
        menu =
            makeMenu $
            \text -> resultId <$ writeIORef textRef text
        menuId x = Widget.Id ["Menu", Text.encodeUtf8 x]

resultId :: Widget.Id
resultId = Widget.Id ["result"]

makeResultWidget ::
    Applicative f => Text -> M.DefaultEnvWithCursor -> M.WithTextPos (Widget.Widget f)
makeResultWidget text =
    do
        toFocusable <- Widget.makeFocusableView
        TextView.make ?? text ?? Widget.toAnimId resultId <&> M.tValue %~ toFocusable resultId

makeMenu :: Applicative f => (Text -> f Widget.Id) -> [Text] -> Widget.Id -> M.DefaultEnvWithCursor -> M.TextWidget f
makeMenu pickText opts menuId =
    SearchMenu.make makeSearchTerm makeResults Element.empty menuId
    ?? SearchMenu.AnyPlace
    where
        makeSearchTerm = SearchMenu.searchTermEdit menuId (const (pure True))
        makeResults (SearchMenu.ResultsContext searchTerm resultIdPrefix) _ =
            SearchMenu.mkOptionList 3 (filteredOpts opts searchTerm)
            <&> toOption resultIdPrefix
        toOption resultIdPrefix text =
            SearchMenu.Option
            { _oId = widgetId
            , _oSubmenuWidgets = SearchMenu.SubmenuEmpty
            , _oRender =
                \e ->
                SearchMenu.RenderedOption
                { SearchMenu._rPick =
                    Widget.PreEvent
                    { Widget._pDesc = "Pick " <> text
                    , Widget._pAction =
                        pickText text <&> \destId ->
                        SearchMenu.PickResult
                        { SearchMenu._pickDest = destId
                        , SearchMenu._pickMNextEntry = Nothing
                        }
                    , Widget._pTextRemainder = ""
                    }
                , SearchMenu._rWidget =
                    Label.make text e <&> Widget.makeFocusableView e widgetId
                }
            }
            where
                widgetId = resultIdPrefix `Widget.joinId` [Text.encodeUtf8 text]
