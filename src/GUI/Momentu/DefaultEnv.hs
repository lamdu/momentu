{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module GUI.Momentu.DefaultEnv where

import qualified Control.Lens as Lens
import           GUI.Momentu.Animation.Id (AnimId)
import qualified GUI.Momentu.Direction as Direction
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as I18N
import qualified GUI.Momentu.Main as Main
import           GUI.Momentu.State (HasCursor, GUIState)
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified GUI.Momentu.Zoom as Zoom

import           GUI.Momentu.Prelude

-- | An environment usable with the default options
data DefaultEnvG guiState = DefaultEnv
    { _commonTexts :: I18N.Texts Text
    , _mainTexts :: Main.Texts Text
    , _choiceTexts :: Choice.Texts Text
    , _textEditTexts :: TextEdit.Texts Text
    , _directionTexts :: Direction.Texts Text
    , _directionLayout :: Direction.Layout
    , _menuTexts :: Menu.Texts Text
    , _menuConfig :: Menu.Config
    , _searchMenuTexts :: SearchMenu.Texts Text
    , _zoomTexts :: Zoom.Texts Text
    , _eventMapTexts :: EventMap.Texts Text
    , _glueTexts :: Glue.Texts Text
    , _helpTexts :: EventMapHelp.Texts Text
    , _animIdPrefix :: AnimId
    , _helpConfig :: EventMapHelp.Config
    , _helpStyle :: EventMapHelp.Style
    , _textEditStyle :: TextEdit.Style
    , _textViewStyle :: TextView.Style
    , _hoverStyle :: Hover.Style
    , _guiState :: guiState
    }

Lens.makeLenses ''DefaultEnvG

type DefaultEnv = DefaultEnvG ()
type DefaultEnvWithCursor = DefaultEnvG GUIState

defaultEnv :: Font -> DefaultEnv
defaultEnv font = DefaultEnv
    { _commonTexts = I18N.englishTexts
    , _mainTexts = Main.englishTexts
    , _choiceTexts = Choice.englishTexts
    , _textEditTexts = TextEdit.englishTexts
    , _directionTexts = Direction.englishTexts
    , _directionLayout = Direction.LeftToRight
    , _menuTexts = Menu.englishTexts
    , _searchMenuTexts = SearchMenu.englishTexts
    , _menuConfig = Menu.defaultConfig
    , _zoomTexts = Zoom.englishTexts
    , _eventMapTexts = EventMap.englishTexts
    , _glueTexts = Glue.englishTexts
    , _helpTexts = EventMapHelp.englishTexts
    , _animIdPrefix = []
    , _helpConfig = EventMapHelp.defaultConfig
    , _helpStyle = EventMapHelp.defaultStyle font
    , _textEditStyle = TextView.whiteText font & TextEdit.defaultStyle
    , _textViewStyle = TextView.whiteText font
    , _hoverStyle = Hover.defaultStyle
    , _guiState = ()
    }

defaultEnvWithCursor :: GUIState -> Font -> DefaultEnvWithCursor
defaultEnvWithCursor state font = defaultEnv font & guiState .~ state

instance Element.HasAnimIdPrefix       (DefaultEnvG a) where animIdPrefix = animIdPrefix
instance Has (Choice.Texts Text)       (DefaultEnvG a) where has = choiceTexts
instance Has (Direction.Texts Text)    (DefaultEnvG a) where has = directionTexts
instance Has (EventMap.Texts Text)     (DefaultEnvG a) where has = eventMapTexts
instance Has (EventMapHelp.Texts Text) (DefaultEnvG a) where has = helpTexts
instance Has (Glue.Texts Text)         (DefaultEnvG a) where has = glueTexts
instance Has (I18N.Texts Text)         (DefaultEnvG a) where has = commonTexts
instance Has (Main.Texts Text)         (DefaultEnvG a) where has = mainTexts
instance Has (Menu.Texts Text)         (DefaultEnvG a) where has = menuTexts
instance Has (SearchMenu.Texts Text)   (DefaultEnvG a) where has = searchMenuTexts
instance Has (TextEdit.Texts Text)     (DefaultEnvG a) where has = textEditTexts
instance Has (Zoom.Texts Text)         (DefaultEnvG a) where has = zoomTexts
instance Has Direction.Layout          (DefaultEnvG a) where has = directionLayout
instance Has EventMapHelp.Config       (DefaultEnvG a) where has = helpConfig
instance Has EventMapHelp.Style        (DefaultEnvG a) where has = helpStyle
instance Has Hover.Style               (DefaultEnvG a) where has = hoverStyle
instance Has Menu.Config               (DefaultEnvG a) where has = menuConfig
instance Has TextEdit.Style            (DefaultEnvG a) where has = textEditStyle
instance Has TextView.Style            (DefaultEnvG a) where has = textViewStyle

instance Has GUIState                  DefaultEnvWithCursor where has = guiState
instance HasCursor                     DefaultEnvWithCursor
