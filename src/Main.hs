{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wname-shadowing -Werror=name-shadowing #-}

module Main where

import Data.Text qualified as T
import HelloCallStack qualified
import Data.Time.Clock
import Data.Time.Calendar
-- import HelloAeson qualified
-- import HelloCryp qualified
-- import HelloDataKinds qualified
-- import HelloEither qualified
-- import HelloEnum qualified
-- import HelloFactors qualified
-- import HelloMaybeT qualified
-- import HelloScientific qualified
-- import HelloStringVariants qualified
-- import HelloTempFile qualified
-- import HelloTime qualified
-- import HelloTypeFamily qualified

-- f :: Show a => Int -> a
-- f _ = "test"

data Foo = Foo
  { a1 :: String
  , a2 :: Int
  }

newtype BarName = BarName String
data Bar = Bar {barName :: BarName}
data Baz = Baz BarName

barz :: Bar -> Baz
barz Bar{barName = barName1} = Baz barName1

bar :: Bar -> Bar
bar Bar{barName = barName2} = Bar{barName = barName2}

main :: IO ()
main = HelloCallStack.main

testSplit :: IO ()
testSplit = do
  print $ T.splitOn "-" ""
  print $ T.splitOn "-" "a"
  print $ T.splitOn "-" "a-b"
  print $ T.splitOn "-" "a-b-"
  print $ T.splitOn "-" "a-b-c"


