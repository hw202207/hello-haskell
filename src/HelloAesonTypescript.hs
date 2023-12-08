{-# LANGUAGE TemplateHaskell #-}

module HelloAesonTypescript where

import Data.Proxy
import Data.Aeson
import Data.Aeson.TypeScript.Internal
import Data.Aeson.TypeScript.TH

data Test = Foo FooData | Bar BarData

newtype FooData = FooData {name::String}

newtype BarData = BarData {location::Int}

$(deriveTypeScript defaultOptions ''FooData)
$(deriveTypeScript defaultOptions ''BarData)
$(deriveTypeScript defaultOptions ''Test)

main :: IO ()
main = putStrLn $
  formatTSDeclarations (
    (getTypeScriptDeclarations (Proxy :: Proxy Test)) <>
    (getTypeScriptDeclarations (Proxy :: Proxy FooData)) <>
    (getTypeScriptDeclarations (Proxy :: Proxy BarData))
  )
