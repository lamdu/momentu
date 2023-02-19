{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module GUI.Momentu.Responsive.TaggedList
    ( TaggedItem(..), tagPre, taggedItem, tagPost
    , taggedListTable, taggedListIndent
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader, pushToReaderExt)
import           Data.Functor.Compose (Compose(..))
import qualified Data.List as List
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Align (WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive
import qualified GUI.Momentu.Responsive.Expression as Expression
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           GUI.Momentu.Prelude

data TaggedItem f = TaggedItem
    { _tagPre :: Maybe (TextWidget f)
    , _taggedItem :: Responsive f
    , _tagPost :: Maybe (TextWidget f)
    }

Lens.makeLenses ''TaggedItem

taggedListTable ::
    ( MonadReader env m, Spacer.HasStdSpacing env, Applicative f
    , Glue.HasTexts env
    ) =>
    [TaggedItem f] -> m (Responsive f)
taggedListTable items =
    Lens.view id <&>
    \env ->
    let idx =
            NarrowLayoutParams
            { _layoutWidth = partWidth (tagPre . Lens._Just) items + partWidth (tagPost . Lens._Just) items
            , _layoutNeedDisambiguation = False
            }
    in
    prepItems items
    & verticalLayout VerticalLayout
    { _vContexts = Lens.reindexed (const idx) Lens.traversed
    , _vLayout = \xs -> makeRenderTable xs WideMultiLine env ^. lWide
    }

prepItems ::
    [TaggedItem f] ->
    Compose [] ((,) (Maybe (TextWidget f), Maybe (TextWidget f))) (Responsive f)
prepItems =
    Compose . map prepItem
    where
        prepItem (TaggedItem pre x post) = ((pre, post), x)

makeOneLiner ::
    (MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Applicative f) =>
    Compose [] ((,) (Maybe (TextWidget f), Maybe (TextWidget f))) (TextWidget f) ->
    WideLayoutForm ->
    m (Maybe (WideLayouts f))
makeOneLiner _ WideMultiLine = pure Nothing
makeOneLiner (Compose xs) WideOneLiner =
    traverse renderItem xs >>= makeHboxSpaced
    <&> (join WideLayouts ?? WideOneLiner) <&> Just
    where
        renderItem ((mPre, mPost), item) =
            do
                rest <-
                    case mPost of
                    Nothing -> pure item
                    Just post -> pure item Glue./|/ pure post
                case mPre of
                    Nothing -> pure rest
                    Just pre -> pure pre Glue./|/ Spacer.stdHSpace Glue./|/ pure rest

makeRenderTable ::
    (MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Applicative f) =>
    Compose [] ((,) (Maybe (TextWidget f), Maybe (TextWidget f))) (TextWidget f) ->
    WideLayoutForm ->
    m (WideLayouts f)
makeRenderTable (Compose xs) partsForm =
    do
        hspace <- Spacer.getSpaceSize <&> (^. _2)
        let preWidth = partWidth (_1 . _1 . Lens._Just) xs + hspace
        items <- traverse (addTableRowPre preWidth) xs
        let itemWidth = partWidth (Lens.filteredBy (_2 . Lens._Just) . _1) items
        res <- traverse (renderTableRow itemWidth) items >>= makeVboxSpaced
        WideLayouts res res form & pure
    where
        form =
            case xs of
            (_:_:_) -> WideMultiLine
            _ -> partsForm

addTableRowPre ::
    (MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Applicative f) =>
    Double -> ((Maybe (TextWidget f), Maybe (TextWidget f)), TextWidget f) ->
    m (TextWidget f, Maybe (TextWidget f))
addTableRowPre preWidth ((mPre, post), item) =
    case mPre of
    Nothing -> pure item
    Just pre ->
        do
            hspace <- Spacer.getSpaceSize <&> (^. _2)
            Element.pad
                (Vector2 (preWidth - pre ^. Element.width - hspace) 0)
                (Vector2 hspace 0) pre
                Glue./|/ pure item
    <&> ((,) ?? post)

renderTableRow ::
    (MonadReader env m, Glue.HasTexts env, Applicative f) =>
    Double -> (TextWidget f, Maybe (TextWidget f)) -> m (TextWidget f)
renderTableRow itemWidth (item, mPost) =
    case mPost of
    Nothing -> pure item
    Just post ->
        pure item Glue./|/
        Element.pad (Vector2 (itemWidth - item ^. Element.width) 0) 0 post

-- TODO: This should be generic and in spacer? If Element had a fromView it could be
makeVboxSpaced ::
    (MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Applicative f) =>
    [TextWidget f] -> m (TextWidget f)
makeVboxSpaced widgets =
    do
        vspace <- Spacer.stdVSpace <&> Widget.fromView <&> WithTextPos 0
        List.intersperse vspace widgets & Glue.vbox

makeHboxSpaced ::
    (MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Applicative f) =>
    [TextWidget f] -> m (TextWidget f)
makeHboxSpaced widgets =
    do
        hspace <- Spacer.stdHSpace <&> Widget.fromView <&> WithTextPos 0
        List.intersperse hspace widgets & Glue.hbox

partWidth :: (Traversable t, Functor f) => Lens.ATraversal' a (TextWidget f) -> t a -> Widget.R
partWidth l = foldl max 0 . (^.. traverse . Lens.cloneTraversal l . Element.width)

-- | Renders a table if there is enough space, otherwise indents the items.
--
-- The pre-tags seperate between indented items (so separation will be lacking without them).
taggedListIndent ::
    ( MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Element.HasElemIdPrefix env
    , Has Expression.Style env, Applicative f
    ) =>
    [TaggedItem f] -> m (Responsive f)
taggedListIndent items =
    do
        table <- pushToReaderExt pushToReader makeRenderTable
        oneLiner <- pushToReaderExt pushToReader makeOneLiner
        Lens.imapM indentedListItem items >>= vboxSpaced
            <&> tryWideLayout (table <&> Lens.mapped %~ Just)
            <&> tryWideLayout oneLiner
    where
        p = prepItems items
        tryWideLayout f =
            Options.tryWideLayout Options.WideLayoutOption
            { Options._wContexts = traverse . Options.wideUnambiguous
            , Options._wLayout = f
            } p

indentedListItem ::
    ( MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Element.HasElemIdPrefix env
    , Has Expression.Style env, Applicative f
    ) =>
    Int -> TaggedItem f -> m (Responsive f)
indentedListItem idx (TaggedItem pre item post) =
    do
        indentPrefix <- Lens.view Element.elemIdPrefix <&> (<> "tagged-item")
        indent <- Expression.indent (indentPrefix <> Anim.asElemId idx) & pushToReader
        i <-
            case post of
            Nothing -> pure item
            Just p -> pure item Glue./|/ pure p
        (pre ^.. Lens._Just <&> fromWithTextPos) <> [vertLayoutMaybeDisambiguate indent i]
            & Options.boxSpaced Options.disambiguationNone
