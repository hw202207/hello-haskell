{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
module Main where

import HelloTypeFamily
import HelloEnum
import HelloDataKinds

-- f :: Show a => Int -> a
-- f _ = "test"

main :: IO ()
main = putStrLn "Hello, Haskell!"
