{-# LANGUAGE DefaultSignatures, ScopedTypeVariables, DerivingStrategies, GeneralisedNewtypeDeriving #-}
module GUI.Momentu.Element.Id
    ( ElemId(..)
    , ElemIds(..)
    , augmentId, subId, isSubId
    ) where

import           Control.DeepSeq (NFData)
import qualified Control.Lens as Lens
import           Data.Binary.Extended (encodeS)
import qualified Data.ByteString.Char8 as SBS8
import           Data.Proxy (Proxy(..))
import           Data.String (IsString(..))
import           Data.Typeable (Typeable, typeRep)

import           GUI.Momentu.Prelude

newtype ElemId =
    ElemId [ByteString]
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Semigroup, Monoid, Binary)
    deriving anyclass NFData

instance IsString ElemId where
    fromString = ElemId . (:[]) . fromString

-- | Global ids for elements in a container of kind `* -> *`.
class ElemIds t where
    elemIds :: t ElemId
    default elemIds :: (Typeable t, Traversable t, Applicative t) => t ElemId
    elemIds =
        let typeStr = SBS8.pack (show (typeRep (Proxy :: Proxy t)))
        in  pure ()
            & Lens.traversed %@~ const . ElemId . (typeStr :) . (:[]) . encodeS

augmentId :: Show a => a -> ElemId -> ElemId
augmentId x animId = animId <> ElemId [show x & SBS8.pack]

subId :: ElemId -> ElemId -> Maybe ElemId
ElemId short `subId` ElemId long = long ^? Lens.prefixed short <&> ElemId

isSubId :: ElemId -> ElemId -> Bool
short `isSubId` long = short `subId` long & Lens.has Lens._Just
