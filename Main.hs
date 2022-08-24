{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
module Main where

newtype UnescapingChar = UnescapingChar {unescapingChar :: Char}

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
