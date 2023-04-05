{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import Data.Text qualified as T
import HelloAeson qualified
import HelloCryp qualified
import HelloDataKinds qualified
import HelloEither qualified
import HelloEnum qualified
import HelloMaybeT qualified
import HelloScientific qualified
import HelloStringVariants qualified
import HelloTime qualified
import HelloTypeFamily qualified
import HelloTempFile qualified

-- f :: Show a => Int -> a
-- f _ = "test"

data Foo = Foo
  { a1 :: String
  , a2 :: Int
  }

main :: IO ()
main = do
  print $ T.splitOn "-" ""
  print $ T.splitOn "-" "a"
  print $ T.splitOn "-" "a-b"
  print $ T.splitOn "-" "a-b-"
  print $ T.splitOn "-" "a-b-c"
