{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wname-shadowing -Werror=name-shadowing #-}

module Main where

import Control.Exception
import Data.Text qualified as T
import GHC.Stack
import HelloAeson qualified
import HelloCallStack qualified
import HelloCryp qualified
import HelloDataKinds qualified
import HelloEither qualified
import HelloEnum qualified
import HelloFactors qualified
import HelloMaybeT qualified
import HelloScientific qualified
import HelloStringVariants qualified
import HelloTempFile qualified
import HelloTime qualified
import HelloTypeFamily qualified

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
main = do
  putStrLn "callstack test1"
  catch
    HelloCallStack.test1
    (\(e :: SomeException) -> do
        putStrLn "Got error"
        print e
        putStrLn "==="
        GHC.Stack.currentCallStack >>= print
    )
  putStrLn "callstack test2"
  HelloCallStack.test2

testSplit :: IO ()
testSplit = do
  print $ T.splitOn "-" ""
  print $ T.splitOn "-" "a"
  print $ T.splitOn "-" "a-b"
  print $ T.splitOn "-" "a-b-"
  print $ T.splitOn "-" "a-b-c"
