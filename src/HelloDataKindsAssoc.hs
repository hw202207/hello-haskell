{-# LANGUAGE LambdaCase, DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HelloDataKindsAssoc where


data Foo (i :: k) = Foo
  { a :: Int
  , b :: String
  }

data Bar = A | B | C
  deriving (Show, Eq, Enum)

f1 :: Foo A
f1 = Foo { a = 1, b = "aaa" }

f2 :: Foo B
f2 = Foo { a = 2, b = "bbb" }

f3 :: Foo C
f3 = Foo { a = 3, b = "ccc" }

-- findFoo :: Bar -> Foo i
-- findFoo = \case
--   A -> f1
--   B -> f2
--   C -> f3
