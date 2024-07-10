{-# LANGUAGE TemplateHaskell #-}

module HelloTHGen where

-- import THUtil

data MyBar = MyBar { fname :: String, fage :: Int }

main :: IO ()
main = print 1
-- main = run $(compileTemplate
--              ''MyBar
--              EmailTemplate{name = "Foo", plainText = "Hello"})
