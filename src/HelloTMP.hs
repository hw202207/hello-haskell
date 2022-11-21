{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-
https://lexi-lambda.github.io/blog/2021/03/25/an-introduction-to-typeclass-metaprogramming/#footnote-ref-3-1

readig notes: my-notes/20221118150351-typeclass_metaprogramming.org

-}

-- | Typeclass Meta Programming
module HelloTMP where

import Numeric.Natural
import Prelude hiding (head, tail)

class TypeOf a where
  typeOf :: String

instance TypeOf Bool where
  typeOf = "Bool"

instance TypeOf Int where
  typeOf = "Int"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf = "(" ++ typeOf @a ++ ", " ++ typeOf @b ++ ")"

data Z
data S a

class ReifyNat a where
  reifyNat :: Natural

instance ReifyNat Z where
  reifyNat = 0
instance ReifyNat a => ReifyNat (S a) where
  reifyNat = 1 + reifyNat @a

type family Sum a b where
  Sum Z b = b
  Sum (S a) b = S (Sum a b)

case1 =
  [ reifyNat @Z
  , reifyNat @(S Z)
  , reifyNat @(S (S Z))
  , reifyNat @(Sum (S (S Z)) (S (S (S Z)))) -- 2 + 3
  , reifyNat @(Sum Z (S Z)) -- 0 + 1
  , reifyNat @(Sum (S Z) Z) -- 1 + 0
  , reifyNat @(Sum Z Z) -- 0 + 0
  ]

type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a] = a

class Flatten a where
  flatten :: a -> [ElementOf a]

instance (ElementOf [a] ~ a) => Flatten [a] where
  -- by definition : flatten ::             [a] -> [ElementOf [a]]
  -- function type : flatten :: a -> a ~~~> [a] -> [a]
  -- flatten x = x
  flatten = id

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  -- flatten x = flatten (concat x)
  flatten = flatten . concat

case2 =
  [ flatten [[[1 :: Integer, 2], [3, 4]], [[5, 6], [7, 8]]]
  , flatten [[[1 :: Integer, 2]], [[4]], [[5, 7], [8, 9]]]
  ]

-- * HList

-- heterogenous list
infixr 5 `HCons`
data HList a where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

head :: HList (a ': as) -> a
head (HCons a _) = a

tail :: HList (a ': as) -> HList as
tail (HCons _ as) = as

list1 = 13 `HCons` True `HCons` ('a', 3) `HCons` 42 `HCons` HNil

-- * PairUp

data Even as where
  EvenNil :: Even '[]
  EvenCons :: Even as -> Even (a ': b ': as)

type family PairUp as where
  PairUp '[] = '[]
  PairUp (a ': b ': as) = (a, b) ': PairUp as

pairUp :: Even as -> HList as -> HList (PairUp as)
pairUp EvenNil HNil = HNil
pairUp (EvenCons as) (a `HCons` b `HCons` ls) = (a, b) `HCons` pairUp as ls

testPairUp =
  pairUp
    (EvenCons $ EvenCons EvenNil)
    (12 `HCons` 'a' `HCons` [3, 5] `HCons` "Hello" `HCons` HNil)

class IsEven as where
  evenP :: Even as

instance IsEven '[] where
  evenP = EvenNil

instance IsEven xs => IsEven (a ': b ': xs) where
  -- Note: how would the right side 'evenP' knows to work on the 'xs' ??
  -- EvenCons :: Even as -> Even a ': b ': as
  --
  -- The type of 'evenP' (left hand side) is
  -- 'Even as' where 'as' is (a ': b ': xs)
  -- hence 'evenP :: Even (a ': b ': xs)'
  -- Given the type of EvenCons, the right side 'evenP'
  -- shall be 'Even xs'
  evenP = EvenCons evenP

pairUp2 :: IsEven as => HList as -> HList (PairUp as)
pairUp2 ls = go evenP ls
 where
  go :: Even as -> HList as -> HList (PairUp as)
  go EvenNil HNil = HNil
  go (EvenCons even) (a `HCons` b `HCons` as) = (a, b) `HCons` go even as

-- * Sub typing

data GqlKind = Both | Input | Output

data ObjectInfo

data GqlType (k :: GqlKind) where
  TScalar :: GqlType 'Both
  TInputObject :: GqlType 'Input
  -- It's TIObject in the blog post but I just change to for "better" looking
  TOutputObject :: ObjectInfo -> GqlType 'Output

data SubKind (k1 :: GqlKind) (k2 :: GqlKind) where
  KRefl :: SubKind k k
  KBoth :: SubKind 'Both k

class IsSubKind k1 k2 where
  subKindProof :: SubKind k1 k2

instance IsSubKind 'Both k where
  subKindProof = KBoth

-- instance (k ~ 'Input) => IsSubKind 'Input k where
instance IsSubKind 'Input 'Input where
  subKindProof = KRefl

-- instance (k ~ 'Output) => IsSubKind 'Output k where
instance IsSubKind 'Output 'Output where
  subKindProof = KRefl

data GqlParser k a

-- | Only Allow 'Input' be input type where actually 'Both' also fits
nullable :: GqlParser 'Input a -> GqlParser 'Input (Maybe a)
nullable = undefined

nullableFixed :: IsSubKind k 'Input => GqlParser k a -> GqlParser k (Maybe a)
nullableFixed = undefined

main :: IO ()
main = do
  mapM_ print case1
  mapM_ print case2
