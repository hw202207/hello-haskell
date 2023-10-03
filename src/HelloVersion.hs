{-# LANGUAGE CPP #-}
module HelloVersion where

helloVersion :: String
helloVersion = VERSION_hello_haskell

cryptoniteVersion :: String
cryptoniteVersion = VERSION_cryptonite

main :: IO ()
main = do
  putStrLn helloVersion
  putStrLn cryptoniteVersion
