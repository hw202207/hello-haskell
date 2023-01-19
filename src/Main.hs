{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import HelloCryp qualified
import HelloDataKinds
import HelloEnum
import HelloTypeFamily
import HelloMaybeT
import HelloAeson
import HelloStringVariants
import HelloTime
import HelloEither

-- f :: Show a => Int -> a
-- f _ = "test"

main :: IO ()
main = do
  HelloCryp.main
