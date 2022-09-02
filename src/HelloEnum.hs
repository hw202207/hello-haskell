{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module HelloEnum where

import Data.Set qualified as Set

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

hasAllFoo :: [Foo] -> Bool
hasAllFoo fs = Set.fromList fs == Set.fromList [minBound..maxBound]

main = do
  print (hasAllFoo $ map foo t1)
  print (hasAllFoo $ map foo $ take 4 t1)
  print (hasAllFoo $ map foo $ take 3 t1)
  print (hasAllFoo $ map foo $ take 2 t1)
