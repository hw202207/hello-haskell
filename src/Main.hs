{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import HelloCryp qualified
import HelloScientific qualified
import HelloDataKinds qualified
import HelloEnum qualified
import HelloTypeFamily qualified
import HelloMaybeT qualified
import HelloAeson qualified
import HelloStringVariants qualified
import HelloTime qualified
import HelloEither qualified
import Data.Text qualified as T

-- f :: Show a => Int -> a
-- f _ = "test"

main :: IO ()
main = do
  HelloScientific.main
  print $ T.splitOn "-" ""
  print $ T.splitOn "-" "a"
  print $ T.splitOn "-" "a-b"
  print $ T.splitOn "-" "a-b-"
  print $ T.splitOn "-" "a-b-c"
