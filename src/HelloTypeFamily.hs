{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
module HelloTypeFamily where

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
type family ToUnescapingTF (a :: k) :: k where
  ToUnescapingTF Char = UnescapingChar
  -- cast to @:: k@ seems not required
  -- ToUnescapingTF (t b :: k) = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF (t b) = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF a = a

-- >>> :kind! ToUnescapingTF Int
-- ToUnescapingTF Int :: Type
-- = Int

-- >>> :kind! ToUnescapingTF Char
-- ToUnescapingTF Char :: Type
-- = UnescapingChar

-- >>> :kind! ToUnescapingTF (Maybe Char)
-- ToUnescapingTF (Maybe Char) :: Type
-- = Maybe UnescapingChar


main :: IO ()
main = putStrLn "Hello, Haskell!"
