{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
module Main where

import HelloTypeFamily
import HelloEnum
import HelloDataKinds
import HelloCryp qualified as  HelloCryp

-- f :: Show a => Int -> a
-- f _ = "test"

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  HelloCryp.main
