{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module HelloEnum where

import Data.List
import Data.Set qualified as Set
import Test.QuickCheck

data Foo = A | B | C | D
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data Bar = Bar
  { foo :: Foo,
    name :: String
  }

t1 :: [Bar]
t1 =
  [ Bar A "aa",
    Bar B "bb",
    Bar C "cc",
    Bar D "dd"
  ]

instance Arbitrary Foo where
  arbitrary = elements [minBound .. maxBound]

hasAllFoo1, hasAllFoo2, hasAllFoo3 :: [Foo] -> Bool
hasAllFoo1 = null . ([minBound .. maxBound] \\)
hasAllFoo2 xs = xs /= [] && (sort $ [minBound .. maxBound] `intersect` xs) == [minBound .. maxBound]
hasAllFoo3 = (== [minBound .. maxBound]) . Set.toList . Set.fromList

prop_hasAllFoo1 :: [Foo] -> Bool
prop_hasAllFoo1 xs =
  hasAllFoo1 xs == hasAllFoo2 xs

prop_hasAllFoo2 :: [Foo] -> Bool
prop_hasAllFoo2 xs =
  hasAllFoo2 xs == hasAllFoo3 xs

prop_hasAllFoo3 :: [Foo] -> Bool
prop_hasAllFoo3 xs =
  hasAllFoo1 xs == hasAllFoo3 xs

main = do
  (quickCheck . withMaxSuccess 1000) prop_hasAllFoo1
  (quickCheck . withMaxSuccess 1000) prop_hasAllFoo2
  (quickCheck . withMaxSuccess 1000) prop_hasAllFoo3
