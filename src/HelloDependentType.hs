{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HelloDependentType where

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Data
import Data.Typeable

-------------------------------------------------------------------------------
--                                 Approach 1                                --
-------------------------------------------------------------------------------

data TplName1 = Foo1 | Bar1 | Baz1
    deriving (Show, Eq)

data FooData1 = FooData1
data BarData1 = BarData1
data BazData1 = BazData1

$(deriveToJSON defaultOptions ''FooData1)
$(deriveToJSON defaultOptions ''BarData1)
$(deriveToJSON defaultOptions ''BazData1)

genTpl1 :: (ToJSON a) => TplName1 -> a -> String
genTpl1 name _tplData = "genTpl1: " ++ show name

-------------------------------------------------------------------------------
--                                 Approach 2                                --
-------------------------------------------------------------------------------

data Foo2
    deriving (Typeable)

data Bar2
    deriving (Typeable)

data Baz2
    deriving (Typeable)

class IsTplName a
instance IsTplName Foo2
instance IsTplName Bar2
instance IsTplName Baz2

type family TplData2 a

type instance TplData2 Foo2 = FooData2
type instance TplData2 Bar2 = BarData2
type instance TplData2 Baz2 = BazData2

data FooData2 = FooData2
data BarData2 = BarData2
data BazData2 = BazData2

$(deriveToJSON defaultOptions ''FooData2)
$(deriveToJSON defaultOptions ''BarData2)
$(deriveToJSON defaultOptions ''BazData2)

genTpl2 :: forall a. (IsTplName a, ToJSON (TplData2 a), Typeable a) => TplData2 a -> String
genTpl2 _tplData = "genTpl2: " ++ show (typeOf @a undefined)

-------------------------------------------------------------------------------
--                                    Approach 3                             --
-------------------------------------------------------------------------------

data TplName3 = Foo3 | Bar3 | Baz3
    deriving (Show, Eq)

data FooData3 = FooData3
data BarData3 = BarData3
data BazData3 = BazData3

type family TplData3 (a :: TplName3)
type instance TplData3 Foo3 = FooData3
type instance TplData3 Bar3 = BarData3
type instance TplData3 Baz3 = BazData3

$(deriveToJSON defaultOptions ''FooData3)
$(deriveToJSON defaultOptions ''BarData3)
$(deriveToJSON defaultOptions ''BazData3)

genTpl3 :: forall a. (ToJSON (TplData3 a)) => TplName3 -> TplData3 a -> String
genTpl3 name _tplData = "genTpl3: " ++ show name

-------------------------------------------------------------------------------
--                                 Approach 4                                --
-------------------------------------------------------------------------------

data TplNamedData where
    Foo4 :: FooData4 -> TplNamedData
    Bar4 :: BarData4 -> TplNamedData
    Baz4 :: BazData4 -> TplNamedData
    deriving (Typeable, Data)

data FooData4 = FooData4 {name :: String, age :: Int}
    deriving (Typeable, Data)
data BarData4 = BarData4
    deriving (Typeable, Data)
data BazData4 = BazData4
    deriving (Typeable, Data)

$(deriveToJSON defaultOptions ''FooData4)
$(deriveToJSON defaultOptions ''BarData4)
$(deriveToJSON defaultOptions ''BazData4)

genTpl4 :: TplNamedData -> String
genTpl4 tnd@(Foo4 tplData) = genTpl4Internal tnd tplData
genTpl4 tnd@(Bar4 tplData) = genTpl4Internal tnd tplData
genTpl4 tnd@(Baz4 tplData) = genTpl4Internal tnd tplData

genTpl4Internal :: (ToJSON a) => TplNamedData -> a -> String
genTpl4Internal tnd td = "genTpl4: " ++ constrOf tnd ++ BSL8.unpack (encode td)

constrOf :: (Data a) => a -> String
constrOf = show . toConstr

-------------------------------------------------------------------------------
--                                    Main                                   --
-------------------------------------------------------------------------------

main :: IO ()
main = do
    mapM_
        print
        [ genTpl1 Foo1 FooData1
        , genTpl1 Bar1 BarData1
        , genTpl1 Baz1 BazData1
        , genTpl1 Foo1 BarData1 -- <---- no compilation error even it doesn't make sense
        , genTpl2 @Foo2 FooData2
        , genTpl2 @Bar2 BarData2
        , genTpl2 @Baz2 BazData2
        , -- , genTpl2 @Foo2 BarData2 -- <---- compilation error
          genTpl3 @Foo3 Foo3 FooData3
        , genTpl3 @Bar3 Bar3 BarData3
        , genTpl3 @Baz3 Baz3 BazData3
        , -- , genTpl3 @Foo3 Foo3 BazData3 -- <--- compilation error
          genTpl4 (Foo4 FooData4{name = "hw", age = 12})
        , genTpl4 (Bar4 BarData4)
        , genTpl4 (Baz4 BazData4)
        -- , genTpl4 (Foo4 BarData4) -- <-- compilation error
        ]
