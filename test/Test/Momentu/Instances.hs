{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Momentu.Instances () where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..))
import           GUI.Momentu.Animation (R)
import qualified GUI.Momentu.Hover as Hover
import           Generic.Random
import           Test.QuickCheck (Arbitrary(..), choose, frequency, getPositive)

import           GUI.Momentu.Prelude

instance Arbitrary Hover.Orientation where
    arbitrary = genericArbitrary uniform

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink (_ :| []) = []
    shrink (x0 :| (x1 : xs)) = (x1 :| xs) : (shrink (x1 : xs) <&> (x0 :|))

instance Arbitrary (Vector2 R) where
    arbitrary =
        Vector2 <$> comp <*> comp
        where
            comp =
                frequency
                [ (1, pure 0)
                , (10, getPositive <$> arbitrary)
                ]

instance Arbitrary a => Arbitrary (Aligned a) where
    arbitrary =
        Aligned
        <$> (Vector2 <$> comp <*> comp)
        <*> arbitrary
        where
            comp =
                frequency
                [ (1, pure 0)
                , (1, pure 1)
                , (10, choose (0, 1))
                ]
