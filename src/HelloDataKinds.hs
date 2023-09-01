{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HelloDataKinds where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Data.Kind
-- import Data.Kind

type family ABD ( a :: k ) :: Type

data Bar a b = Bar {x :: a, y :: b}

f1 :: forall a b. ( b ~ ABD a) =>  Bar a b -> IO ()
f1 _ = do
  let _val = (Proxy @b)
  pure ()

type instance ABD Char = Bool
test :: IO ()
test = f1 (Bar {x = 'a', y = True})


data IDP n = IDP

-- data IDP (n :: Symbol) = IDP

-- auth0 :: IDP "auth0"
auth0 :: IDP "auth0"
auth0 = IDP

okta :: IDP "okta"
okta = IDP

-- compile error of course
-- idps = [auth0, okta]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print (toGrantParam2 (undefined :: AuthRequest 'Password))

-- data Foo = forall n. KnownSymbol n => Foo (Int, n)
symbolVal2 :: forall n. (KnownSymbol n) => IDP n -> String
symbolVal2 = undefined

data GT = Code | Password | Client | Foo

class ToGrantParam (a :: GT) where
  toGrantParam :: String

instance ToGrantParam 'Code where
  toGrantParam = "code"

instance ToGrantParam 'Password where
  toGrantParam = "password"

class ToGrantParam2 a where
  toGrantParam2 :: a -> String

instance (ToGrantParam a) => ToGrantParam2 (AuthRequest a) where
  toGrantParam2 _ = toGrantParam @a

toGrantParam3 :: forall a req. (ToGrantParam a) => req a -> String
toGrantParam3 _ = toGrantParam @a

data family AuthRequest (a :: GT)

data instance AuthRequest 'Code = CodeAuthRequest
  { appName :: String
  , redirectUri :: RedirectUri
  }
  deriving (Generic)

newtype instance AuthRequest 'Password = PasswordAuthRequest
  { appName :: String
  }

nonsense :: AuthRequest a -> String
nonsense _ = "et"

requestGrantParam :: forall a. (ToGrantParam a) => AuthRequest a -> String
requestGrantParam _ = toGrantParam @a

newtype RedirectUri = RedirectUri {unRedirectUri :: String}
  deriving (Show)

codeReq :: AuthRequest 'Code
codeReq =
  CodeAuthRequest
    { appName = "demo app"
    , redirectUri = RedirectUri "http://localhost"
    }

-- x =
--   M1
--     { unM1 =
--         M1
--           { unM1 =
--               M1 { unM1 = K1 {unK1 = "demo app"}}
--               :*: M1 { unM1 = K1
--                         { unK1 = RedirectUri {unRedirectUri = "http://localhost"}
--                         }
--                      }
--           }
--     }

mkCodeUri :: AuthRequest 'Code -> [(String, String)]
mkCodeUri req@CodeAuthRequest{..} =
  [ ("name", appName)
  , ("redirect_uri", unRedirectUri redirectUri)
  , ("grant_type", requestGrantParam req)
  ]

passwordReq :: AuthRequest 'Password
passwordReq =
  PasswordAuthRequest
    { appName = "demo app"
    }
