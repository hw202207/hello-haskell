{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
module HelloTypeFamily where

import GHC.Types
-- import Data.Proxy
import GHC.Num.Natural

newtype UnescapingChar = UnescapingChar {unescapingChar :: Char}

-- | how to interpret @(a :: k) :: k@?
-- @
-- ( ToUnescapingTF (a :: k) ) :: k
-- @
-- Why need this? I think the reason being is because
-- @a@ can be both 'Type' and @'Type' -> 'Type'@
-- due to the following implementation
-- @ToUnescapingTF t@
-- @ToUnescapingTF b@
-- where @t@ has kind @Type -> Type@
-- and @b@ has kind @Type@
--

{-
FIXME: unable to compile at GHC2021/GHC-9.4.7
type family ToUnescapingTF (a :: k) :: k where
  ToUnescapingTF Char = UnescapingChar
  -- cast to @:: k@ seems not required
  -- ToUnescapingTF (t b :: k) = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF (t b ) = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF a = a
-}

-- >>> :kind! ToUnescapingTF Int
-- ToUnescapingTF Int :: Type
-- = Int

-- >>> :kind! ToUnescapingTF Char
-- ToUnescapingTF Char :: Type
-- = UnescapingChar

-- >>> :kind! ToUnescapingTF (Maybe Char)
-- ToUnescapingTF (Maybe Char) :: Type
-- = Maybe UnescapingChar


type family FooS (a :: Symbol) :: Type where
  FooS "a" = Char
  FooS "b" = Bool
  FooS "c" = Int

type family FooN (a :: Natural) :: Type where
  FooN 1 = Char
  FooN 2 = Bool
  FooN 3 = Int

data Foo ( a :: Type ) = MkFoo a Bool

a :: Foo Int
a = MkFoo 3 True

b :: Foo (FooS "b")
b = MkFoo False True

c :: Foo (FooN 3)
c = MkFoo 2 True

main :: IO ()
main = putStrLn "Hello, Haskell!"
