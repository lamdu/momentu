{-# LANGUAGE TemplateHaskell #-}

module GUI.Momentu.Responsive.TaggedList
    ( TaggedItem(..), tagPre, taggedItem, tagPost
    , taggedListTable, taggedListIndent
    ) where

import qualified Control.Lens as Lens
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
    m ([TaggedItem f] -> Responsive f)
taggedListTable =
    makeRenderTable <&>
    \renderItems items ->
    let idx =
            NarrowLayoutParams
            { _layoutWidth = partWidth (tagPre . Lens._Just) items + partWidth (tagPost . Lens._Just) items
            , _layoutNeedDisambiguation = False
            }
    in
    prepItems items
    & verticalLayout VerticalLayout
    { _vContexts = Lens.reindexed (const idx) Lens.traversed
    , _vLayout = renderItems
    }

prepItems :: [TaggedItem f] -> Compose [] ((,) (Maybe (TextWidget f), Maybe (TextWidget f))) (Responsive f)
prepItems =
    Compose . map prepItem
    where
        prepItem (TaggedItem pre x post) = ((pre, post), x)

makeRenderTable ::
    (MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Applicative f) =>
    m (Compose [] ((,) (Maybe (TextWidget f), Maybe (TextWidget f))) (TextWidget f) -> TextWidget f)
makeRenderTable =
    do
        doPad <- Element.pad
        (/|/) <- Glue.mkGlue ?? Glue.Horizontal
        vboxed <- makeVboxSpaced
        hspace <- Spacer.getSpaceSize <&> (^. _2)
        pure (
            \(Compose xs) ->
            let items = xs <&> addPre
                addPre ((Nothing, post), item) = (item, post)
                addPre ((Just pre, post), item) =
                    ( doPad
                        (Vector2 (preWidth - pre ^. Element.width - hspace) 0)
                        (Vector2 hspace 0) pre
                        /|/ item
                    , post
                    )
                preWidth = partWidth (_1 . _1 . Lens._Just) xs + hspace
                itemWidth = partWidth (Lens.filteredBy (_2 . Lens._Just) . _1) items
                renderRow (item, Nothing) = item
                renderRow (item, Just post) =
                    item /|/
                    doPad (Vector2 (itemWidth - item ^. Element.width) 0) 0 post
            in items <&> renderRow & vboxed)

-- TODO: This should be generic and in spacer? If Element had a fromView it could be
makeVboxSpaced ::
    (MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Applicative f) =>
    m ([TextWidget f] -> TextWidget f)
makeVboxSpaced =
    do
        vboxed <- Glue.vbox
        vspace <- Spacer.stdVSpace <&> Widget.fromView <&> WithTextPos 0
        vboxed . List.intersperse vspace & pure

partWidth :: (Traversable t, Functor f) => Lens.ATraversal' a (TextWidget f) -> t a -> Widget.R
partWidth l = foldl max 0 . (^.. traverse . Lens.cloneTraversal l . Element.width)

-- | Renders a table if there is enough space, otherwise indents the items.
--
-- The pre-tags seperate between indented items (so separation will be lacking without them).
taggedListIndent ::
    ( MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Element.HasAnimIdPrefix env
    , Has Expression.Style env, Applicative f
    ) =>
    m ([TaggedItem f] -> Responsive f)
taggedListIndent =
    do
        vboxed <- vboxSpaced
        mkItem <- indentedListItem
        table <- makeRenderTable
        pure (
            \items ->
            Lens.imap mkItem items & vboxed
            & Options.tryWideLayout Options.WideLayoutOption
                { Options._wContexts = traverse . Lens.iso id (^. lWide)
                , Options._wLayout = join WideLayouts . table
                } (prepItems items)
            )

indentedListItem ::
    ( MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Element.HasAnimIdPrefix env
    , Has Expression.Style env, Applicative f
    ) =>
    m (Int -> TaggedItem f -> Responsive f)
indentedListItem =
    do
        box <- Options.boxSpaced ?? Options.disambiguationNone
        (/|/) <- Glue.mkGlue ?? Glue.Horizontal
        indentPrefix <- Lens.view Element.animIdPrefix <&> (<> ["tagged-item"])
        indent <- Expression.indent
        pure (
            \idx (TaggedItem pre item post) ->
            (pre ^.. Lens._Just <&> fromWithTextPos)
            <> [vertLayoutMaybeDisambiguate (indent (Anim.augmentId idx indentPrefix))
                (maybe id (flip (/|/)) post item)]
            & box
            )
