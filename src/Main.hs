{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
module Main where

import HelloTypeFamily
import HelloEnum

main :: IO ()
main = putStrLn "Hello, Haskell!"
