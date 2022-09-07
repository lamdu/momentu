module Data.List.Extended.Momentu
    ( module Data.List
    , groupOn
    , minimumOn
    ) where

import           Data.Function (on)
import           Data.List
import           Data.Ord (comparing)

import           Prelude

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . comparing
