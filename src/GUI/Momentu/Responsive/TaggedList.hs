{-# LANGUAGE TemplateHaskell #-}

module GUI.Momentu.Responsive.TaggedList
    ( TaggedItem(..), tagPre, taggedItem, tagPost
    , taggedList
    ) where

import qualified Control.Lens as Lens
import           Data.Functor.Compose (Compose(..))
import qualified Data.List as List
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           GUI.Momentu.Prelude

data TaggedItem f = TaggedItem
    { _tagPre :: Maybe (TextWidget f)
    , _taggedItem :: Responsive f
    , _tagPost :: Maybe (TextWidget f)
    }

Lens.makeLenses ''TaggedItem

taggedList ::
    ( MonadReader env m, Spacer.HasStdSpacing env, Applicative f
    , Glue.HasTexts env
    ) =>
    m ([TaggedItem f] -> Responsive f)
taggedList =
    do
        doPad <- Element.pad
        (/|/) <- Glue.mkGlue ?? Glue.Horizontal
        renderItems <- makeRenderTable
        pure (
            \items ->
            let preWidth = partWidth (tagPre . Lens._Just) items
                renderItem ((Nothing, post), item) = (item, post)
                renderItem ((Just pre, post), item) =
                    ( doPad (Vector2 (preWidth - pre ^. Element.width) 0) 0 pre
                        /|/ item
                    , post
                    )
                idx =
                    NarrowLayoutParams
                    { _layoutWidth = preWidth + partWidth (tagPost . Lens._Just) items
                    , _layoutNeedDisambiguation = False
                    }
            in
            items <&> prepItem & Compose
            & verticalLayout VerticalLayout
            { _vContexts = Lens.reindexed (const idx) Lens.traversed
            , _vLayout = renderItems . map renderItem . getCompose
            }
            )
    where
        prepItem (TaggedItem pre x post) = ((pre, post), x)

makeRenderTable ::
    ( MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env, Applicative f) =>
    m ([(TextWidget f, Maybe (TextWidget f))] -> TextWidget f)
makeRenderTable =
    do
        doPad <- Element.pad
        (/|/) <- Glue.mkGlue ?? Glue.Horizontal
        vboxed <- Glue.vbox
        vspace <- Spacer.stdVSpace <&> Widget.fromView <&> WithTextPos 0
        pure (
            \xs ->
            let itemWidth = partWidth (Lens.filteredBy (_2 . Lens._Just) . _1) xs
                renderRow (item, Nothing) = item
                renderRow (item, Just post) =
                    item /|/
                    doPad (Vector2 (itemWidth - item ^. Element.width) 0) 0 post
            in xs <&> renderRow & List.intersperse vspace & vboxed)

partWidth :: (Traversable t, Functor f) => Lens.ATraversal' a (TextWidget f) -> t a -> Widget.R
partWidth l = foldl max 0 . (^.. traverse . Lens.cloneTraversal l . Element.width)
