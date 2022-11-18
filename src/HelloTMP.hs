{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- https://lexi-lambda.github.io/blog/2021/03/25/an-introduction-to-typeclass-metaprogramming/#footnote-ref-3-1

-- | Typeclass Meta Programming
module HelloTMP where

import Numeric.Natural

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
    flatten x = x

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
    flatten x = flatten (concat x)

case2 =
    [ flatten [[[1 :: Integer, 2], [3, 4]], [[5, 6], [7, 8]]]
    , flatten [[[1 :: Integer, 2]], [[4]], [[5, 7], [8, 9]]]
    ]

main = do
    mapM print case1
    mapM print case2
