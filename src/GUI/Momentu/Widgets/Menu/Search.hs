{-# LANGUAGE TemplateHaskell, DerivingVia, ConstraintKinds #-}

module GUI.Momentu.Widgets.Menu.Search
    ( emptyPickEventMap
    , resultsIdPrefix
    , ResultsContext(..), rSearchTerm, rResultIdPrefix

    , basicSearchTermEdit
    , defaultEmptyStrings
    , addPickFirstResultEvent
    , addSearchTermBgColor, addSearchTermEmptyColors
    , addDelSearchTerm
    , searchTermEdit

    , Config(..), configMenu
    , HasConfig
    , OSString, defaultConfig

    , TermStyle(..), bgColors, emptyStrings, emptyStringsColors
        , defaultTermStyle
    , enterWithSearchTerm
    , Term(..), termWidget, termEditEventMap

    , TermCtx(..), tcTextEdit, tcAdHoc
    , AllowedSearchTerm

    , Menu.Placement(..)
    , Menu.OptionList(..)
    , make

    , readSearchTerm

    , Texts(..)
        , textPickNotApplicable, textSearchTerm
        , textAppendChar, textDeleteBackwards
        , textOpenResults, textCloseResults
    , englishTexts

    , Deps
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified Data.Text as Text
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (OSString, cmd)
import           GUI.Momentu.ModKey (ModKey(..), noMods)
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.State (HasState)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widget.Id (Id(..), joinId)
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView

import           GUI.Momentu.Prelude

data WidgetState = WidgetState
    { _wSearchTermText :: !Text
    , __wIsOpen :: !Bool
    } deriving (Generic, Show)
Lens.makeLenses ''WidgetState
instance Binary WidgetState

data Texts a = Texts
    { _textPickNotApplicable :: a
    , _textSearchTerm :: a
    , _textAppendChar :: a
    , _textDeleteBackwards :: a
    , _textOpenResults :: a
    , _textCloseResults :: a
    } deriving Eq
Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_text" ''Texts

data Config key = Config
    { _configMenu :: Menu.Config key
    , _configOpenKeys :: [key]
    , _configCloseKeys :: [key]
    , _configDelSearchTermKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
Lens.makeLenses ''Config
JsonTH.derivePrefixed "_config" ''Config

defaultConfig :: OSString -> Config ModKey
defaultConfig os = Config
    { _configMenu = Menu.defaultConfig
    , _configOpenKeys = [noMods ModKey.Key'Enter]
    , _configCloseKeys = [noMods ModKey.Key'Escape]
    , _configDelSearchTermKeys = [cmd os ModKey.Key'Backspace]
    }

type HasConfig env = (Has (Config ModKey) env, Has (Menu.Config ModKey) env)

englishTexts :: Texts Text
englishTexts = Texts
    { _textPickNotApplicable = "Pick (N/A)"
    , _textSearchTerm = "Search Term"
    , _textAppendChar = "Append character"
    , _textDeleteBackwards = "Delete backwards"
    , _textOpenResults = "Open results"
    , _textCloseResults = "Close results"
    }

type HasTexts env =
    ( Has (Menu.Texts Text) env, Has (Texts Text) env
    , Has (MomentuTexts.Texts Text) env
    )

-- | Context necessary for creation of menu items for a search.
data ResultsContext = ResultsContext
    { _rSearchTerm :: Text
    , _rResultIdPrefix :: Id
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''ResultsContext

data TermStyle = TermStyle
    { _bgColors :: TextEdit.Modes Draw.Color
    , _emptyStrings :: TextEdit.EmptyStrings
    , _emptyStringsColors :: TextEdit.Modes Draw.Color
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_"''TermStyle

defaultTermStyle :: TermStyle
defaultTermStyle = TermStyle
    { _bgColors =
        TextEdit.Modes
        { TextEdit._unfocused = Draw.Color 0.25 0.28 0.50 1.0
        , TextEdit._focused   = Draw.Color 0.30 0.50 0.25 1.0
        }
    , _emptyStrings =
        TextEdit.Modes
        { TextEdit._unfocused = "?"
        , TextEdit._focused   = "_"
        }
    , _emptyStringsColors =
        TextEdit.Modes
        { TextEdit._focused   = Draw.Color 0.7 0.7 0.7 1.0
        , TextEdit._unfocused = Draw.Color 1.0 1.0 1.0 1.0
        }
    }

Lens.makeLenses ''TermStyle

type HasStyle env = (Has TermStyle env, Has Menu.Style env, Has TextView.Style env, Has Hover.Style env)

data Term f = Term
    { _termWidget :: TextWidget f
    , _termEditEventMap :: EventMap (f State.Update)
    }
Lens.makeLenses ''Term

data TermCtx a = TermCtx
    { _tcTextEdit :: a
    , _tcAdHoc :: a
    } deriving stock (Generic, Generic1, Functor, Foldable, Traversable)
    deriving Applicative via Generically1 TermCtx
Lens.makeLenses ''TermCtx

-- | Whether search term is allowed in each search term context:
type AllowedSearchTerm = Text -> TermCtx Bool

emptyPickEventMap ::
    ( MonadReader env m, HasConfig env, Has (Texts Text) env
    , Applicative f
    ) =>
    m (EventMap (f State.Update))
emptyPickEventMap =
    Lens.view id
    <&> \env ->
    E.keysEventMap (allPickKeys env)
    (E.toDoc env [has . textPickNotApplicable]) (pure ())
    where
        allPickKeys env =
            config ^. Menu.configKeysPickOption <>
            config ^. Menu.configKeysPickOptionAndGotoNext
            where
                config = env ^. has

-- | All search menu results must start with a common prefix.
-- This is used to tell when cursor was on a result that got filtered out
-- when the search term changed in order to redirect it to a result.
resultsIdPrefix :: Id -> Id
resultsIdPrefix = (`joinId` ["Results"])

searchTermEditId :: Id -> Id
searchTermEditId = (`joinId` ["SearchTerm"])

readState :: (MonadReader env m, HasState env) => Id -> m WidgetState
readState menuId = State.readWidgetState menuId <&> fromMaybe (WidgetState "" False)

updateWidgetState :: Id -> WidgetState -> State.Update
updateWidgetState = State.updateWidgetState

readSearchTerm :: (MonadReader env m, HasState env) => Id -> m Text
readSearchTerm = readState <&> Lens.mapped %~ (^. wSearchTermText)

defaultEmptyStrings :: TextEdit.EmptyStrings
defaultEmptyStrings = TextEdit.Modes "  " "  "

-- | Basic search term edit:
--   * no bg color --> use addSearchTermBgColor to add it
--   * no empty colors --> use addSearchTermEmptyColors to add it
--   * no pick of first result / no Has Menu.Config needed --> use
--     addPickFirstResultEvent to add it
basicSearchTermEdit ::
    ( MonadReader env m, Applicative f, HasTexts env
    , TextEdit.Deps env, HasState env
    ) =>
    AnimId -> Id -> AllowedSearchTerm -> TextEdit.EmptyStrings -> m (Term f)
basicSearchTermEdit searchTermId menuId rawAllowedSearchTerm textEditEmpty =
    do
        searchTerm <- readSearchTerm menuId
        let onEvents (newSearchTerm, eventRes)
                | newSearchTerm == searchTerm = eventRes
                | otherwise =
                    eventRes
                    <> updateWidgetState menuId (WidgetState newSearchTerm True)
                    -- When first letter is typed in search term, jump to the
                    -- results, which will go to first result:
                    & if Text.null searchTerm
                        then
                            State.uCursor .~
                            Just (resultsIdPrefix menuId) ^. Lens._Unwrapped
                        else id
        widget <-
            TextEdit.makeWithAnimId ?? textEditEmpty ?? searchTerm ?? searchTermId ?? searchTermEditId menuId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                E.filter (_tcTextEdit . allowedSearchTerm . fst)
            <&> Align.tValue . Widget.updates %~ pure . onEvents
            <&> Align.tValue %~ Widget.takesStroll menuId
        env <- Lens.view id
        pure Term
            { _termWidget = widget
            , _termEditEventMap =
                searchTermEditEventMap env menuId (_tcAdHoc . allowedSearchTerm) searchTerm
            }
    where
        allowedSearchTerm text
            | "\n" `Text.isInfixOf` text = pure False
            | otherwise = rawAllowedSearchTerm text

searchTermDoc :: HasTexts env => env -> Lens.ALens' env Text -> E.Doc
searchTermDoc env subtitle =
    E.toDoc env [has . MomentuTexts.edit, has . textSearchTerm, subtitle]

addDelSearchTerm ::
    ( MonadReader env m, State.HasState env, HasTexts env, Has (Config ModKey) env, Applicative f
    ) =>
    Id -> m (Term f -> Term f)
addDelSearchTerm menuId =
    Lens.view id
    <&>
    \env term ->
    let searchTerm = readSearchTerm menuId env
        delSearchTerm
            | Text.null searchTerm = mempty
            | otherwise =
                enterWithSearchTerm "" menuId & pure
                & E.keyPresses (env ^. has . configDelSearchTermKeys)
                (searchTermDoc env (has . MomentuTexts.delete))
    in  term
        & termWidget . Align.tValue %~ Widget.weakerEventsWithoutPreevents delSearchTerm
        & termEditEventMap <>~ delSearchTerm

viewDoc :: HasTexts env => env -> Lens.ALens' env Text -> E.Doc
viewDoc env subtitle = E.toDoc env [has . MomentuTexts.view, subtitle]

addSearchTermBgColor ::
    ( MonadReader env m, State.HasCursor env, Has TermStyle env, Functor f
    ) => Id -> m (TextWidget f -> TextWidget f)
addSearchTermBgColor menuId =
    do
        isActive <- State.isSubCursor ?? menuId
        bgColor <-
            Lens.view
            (has . bgColors .
                if isActive then TextEdit.focused else TextEdit.unfocused)
        Draw.backgroundColor bgAnimId bgColor & pure
    where
        bgAnimId = Widget.toAnimId menuId <> ["hover background"]

addSearchTermEmptyColors ::
    ( MonadReader env m, Has TermStyle env, Has TextEdit.Style env
    ) =>
    m a -> m a
addSearchTermEmptyColors act =
    do
        colors <- Lens.view (has . emptyStringsColors)
        Reader.local (has . TextEdit.sEmptyStringsColors .~ colors) act

searchTermEdit ::
    ( MonadReader env m, Applicative f, Has TermStyle env
    , TextEdit.Deps env, HasConfig env, State.HasState env
    , HasTexts env
    ) =>
    Widget.Id -> (Text -> TermCtx Bool) -> Menu.PickFirstResult f -> m (Term f)
searchTermEdit menuId allowedSearchTerm mPickFirst =
    ( (.)
        <$> addPickFirstResultEvent mPickFirst
        <*> addSearchTermBgColor menuId
        <&> (termWidget %~)
    )
    <*>
        ( Lens.view (has . emptyStrings)
            >>= basicSearchTermEdit (searchTermEditId menuId & Widget.toAnimId) menuId allowedSearchTerm
            & (addDelSearchTerm menuId <*>)
        )
    & addSearchTermEmptyColors

-- Add events on search term to pick the first result.
addPickFirstResultEvent ::
    (MonadReader env m, HasConfig env, HasTexts env, Applicative f) =>
    Menu.PickFirstResult f ->
    m (TextWidget f -> TextWidget f)
addPickFirstResultEvent mPickFirst =
    case mPickFirst of
    Menu.NoPickFirstResult -> emptyPickEventMap
    Menu.PickFirstResult pickFirst -> Menu.makePickEventMap ?? pickFirst
    <&> (Align.tValue %~) . Widget.weakerEvents

assignCursor ::
    (MonadReader env m, HasState env) =>
    Id -> [Id] -> m a -> m a
assignCursor menuId resultIds action =
    do
        searchTerm <- readSearchTerm menuId
        let destId
                | Text.null searchTerm =
                    -- When entering a menu with an empty search string
                    -- (Like after typing "factorial x="),
                    -- cursor should be on the search-string and not on a result
                    -- so that operators pressed will set the search string
                    -- rather than apply on the first result.
                    searchTermEditId menuId
                | otherwise =
                      resultIds ^? Lens.traverse
                      & fromMaybe (Menu.noResultsId menuId)

        -- Results appear and disappear when the search-string changes,
        -- but the cursor prefix signifies whether we should be on a result.
        -- When that is the case but is not currently on any of the existing results
        -- the cursor will be sent to the default one.
        shouldBeOnResult <- sub (resultsIdPrefix menuId)
        isOnResult <- traverse sub resultIds <&> or

        action
            & if shouldBeOnResult && not isOnResult
            then Reader.local (State.cursor .~ destId)
            else State.assignCursor menuId destId
    where
        sub x = State.isSubCursor ?? x

enterWithSearchTerm :: Text -> Id -> State.Update
enterWithSearchTerm searchTerm menuId =
    State.updateCursor menuId
    <> updateWidgetState menuId (WidgetState searchTerm True)

type Deps env =
    ( HasTexts env, HasState env, HasConfig env
    , HasStyle env, Element.HasAnimIdPrefix env
    , HasTexts env, Glue.HasTexts env
    )

make ::
    (MonadReader env m, Applicative f, Deps env) =>
    (Menu.PickFirstResult f -> m (Term f)) ->
    (ResultsContext -> m (Menu.OptionList (Menu.Option m f))) ->
    View -> Id ->
    m (Menu.Placement -> TextWidget f)
make makeSearchTerm makeOptions ann menuId =
    do
        WidgetState searchTerm isOpen <- readState menuId

        env <- Lens.view id
        let setIsOpen x = updateWidgetState menuId . WidgetState searchTerm $ x
        let closeEventMap goto =
                setIsOpen False <> goto
                & pure & E.keyPresses (closeKeys env) (viewDoc env (has . textCloseResults))
        let openEventMap =
                setIsOpen True & pure
                & E.keyPresses (openKeys env) (viewDoc env (has . textOpenResults))

        isSelected <- State.isSubCursor ?? menuId
        (mPickFirst, toMenu, assignTheCursor) <-
            if isSelected
            then if isOpen
                then do
                    options <-
                        ResultsContext searchTerm (resultsIdPrefix menuId) & makeOptions
                    let assignTheCursor = assignCursor menuId (options ^.. traverse . Menu.oId)
                    (mPickFirst, makeMenu) <- Menu.makeHovered menuId ann options & assignTheCursor
                    let makeTheMenu term placement =
                            Widget.weakerEventsWithoutPreevents (closeEventMap mempty)
                            <&> makeMenu placement
                            ( Align.tValue %~
                                Widget.weakerEventsWithoutPreevents (closeEventMap gotoSearchTerm) .
                                Widget.strongerEventsWithoutPreevents (term ^. termEditEventMap)
                            ) <&> assertFocused
                    pure (mPickFirst, makeTheMenu, assignTheCursor)
                else
                    pure
                    ( Menu.NoPickFirstResult
                    , (const . const . Widget.strongerEvents) openEventMap
                    , assignCursor menuId []
                    )
            else
                pure (Menu.NoPickFirstResult, const (const id), assignCursor menuId [])
        makeSearchTerm mPickFirst
            <&> (\term placement -> term ^. termWidget & Align.tValue %~ toMenu term placement)
            <&> Lens.mapped . Lens.mapped . Widget.enterResultCursor .~ menuId
            & Reader.local (Element.animIdPrefix .~ toAnimId menuId)
            & assignTheCursor
    where
        openKeys env = env ^. has . configOpenKeys
        closeKeys env = env ^. has . configCloseKeys
        gotoSearchTerm = searchTermEditId menuId & State.updateCursor
        assertFocused w
            | Widget.isFocused w = w
            | otherwise =
                "Cursor is under Menu prefix " <> show menuId <> " but not on search term or any result"
                & error

searchTermEditEventMap ::
    (Applicative f, HasTexts env) =>
    env -> Id -> (Text -> Bool) -> Text -> EventMap (f State.Update)
searchTermEditEventMap env menuId allowedTerms searchTerm =
    appendCharEventMap <> deleteCharEventMap
    <&> (`WidgetState` True)
    <&> updateWidgetState menuId
    <&> pure
    where
        appendCharEventMap =
            Text.snoc searchTerm
            & E.allChars "Character" (searchTermDoc env (has . textAppendChar))
            -- We only filter when appending last char, not when
            -- deleting last char, because after appending deletion
            -- necessarily preserves any invariant we enforce in
            -- allowedTerms
            & E.filter allowedTerms
        deleteCharEventMap
            | Text.null searchTerm = mempty
            | otherwise =
                Text.init searchTerm
                & E.keyPress (noMods ModKey.Key'Backspace)
                    (searchTermDoc env (has . textDeleteBackwards))
