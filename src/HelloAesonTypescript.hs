{-# LANGUAGE TemplateHaskell #-}

module HelloAesonTypescript where

import Data.Aeson
import Data.Aeson.TypeScript.Internal
import Data.Aeson.TypeScript.TH
import Data.Proxy

data Test = Foo FooData | Bar BarData | Baz BazData

newtype Common = MkCommon {name :: String}

newtype FooData = FooData {fcommon :: Common}

newtype BarData = BarData {location :: Int}

newtype BazData = BazData {bzCommon :: Common}

$(deriveTypeScript defaultOptions ''Common)
$(deriveTypeScript defaultOptions ''FooData)
$(deriveTypeScript defaultOptions ''BarData)
$(deriveTypeScript defaultOptions ''BazData)
$(deriveTypeScript defaultOptions ''Test)

main :: IO ()
main =
    putStrLn
        $ formatTSDeclarations
            ( (getTypeScriptDeclarations (Proxy :: Proxy Test))
                <> (getTypeScriptDeclarations (Proxy :: Proxy FooData))
                <> (getTypeScriptDeclarations (Proxy :: Proxy BarData))
                <> (getTypeScriptDeclarations (Proxy :: Proxy BazData))
                <> (getTypeScriptDeclarations (Proxy :: Proxy Common))
            )
