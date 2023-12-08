{-# LANGUAGE TemplateHaskell #-}

module HelloTHGen where

import TH

main :: IO ()
main = run $(compileTemplate EmailTemplate{name = "Foo", plainText = "Hello"})
