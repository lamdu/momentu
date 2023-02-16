{-# LANGUAGE DefaultSignatures, ScopedTypeVariables #-}
module GUI.Momentu.Element.Id
    ( ElemId
    , ElemIds(..)
    , augmentId, subId, isSubId
    ) where

import qualified Control.Lens as Lens
import           Data.Binary.Extended (encodeS)
import qualified Data.ByteString.Char8 as SBS8
import           Data.Proxy (Proxy(..))
import           Data.Typeable (Typeable, typeRep)

import           GUI.Momentu.Prelude

type ElemId = [ByteString]

-- | Global ids for elements in a container of kind `* -> *`.
class ElemIds t where
    elemIds :: t ElemId
    default elemIds :: (Typeable t, Traversable t, Applicative t) => t ElemId
    elemIds =
        let typeStr = SBS8.pack (show (typeRep (Proxy :: Proxy t)))
        in  pure ()
            & Lens.traversed %@~ const . (typeStr :) . (:[]) . encodeS

augmentId :: Show a => a -> ElemId -> ElemId
augmentId x animId = animId ++ [show x & SBS8.pack]

subId :: ElemId -> ElemId -> Maybe ElemId
short `subId` long = long ^? Lens.prefixed short

isSubId :: ElemId -> ElemId -> Bool
short `isSubId` long = short `subId` long & Lens.has Lens._Just
