{-# LANGUAGE ScopedTypeVariables #-}
module HelloCallStack where

import Control.Exception
import GHC.Stack

foo :: (HasCallStack) => Int -> String
foo = error "oh no"

bar :: (HasCallStack) => Int -> String
bar = foo . negate

baz :: (HasCallStack) => Int -> String
baz = bar . (* 2)

test2 :: (HasCallStack) => IO ()
test2 = print $ baz 5

a :: Int -> String
a = error "oh no..."

b :: Int -> String
b = a . negate

c :: Int -> String
c = b . (* 2)

test1 :: IO ()
test1 = print (c 11)

main :: IO ()
main = do
    putStrLn "== callstack test without HasCallStack =="
    catch
        HelloCallStack.test1
        ( \(e :: SomeException) -> do
            putStrLn "Got error"
            print e
            putStrLn "~ GHC.Stack.currentCallStack ~"
            GHC.Stack.currentCallStack >>= mapM_ putStrLn
        )
    putStrLn "== callstack test with HasCallStack =="
    catch
        HelloCallStack.test2
        ( \(e :: SomeException) -> do
            print e
            putStrLn "~ GHC.Stack.currentCallStack ~"
            GHC.Stack.currentCallStack >>= mapM_ putStrLn
        )
