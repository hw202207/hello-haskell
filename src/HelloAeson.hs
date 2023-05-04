{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, ImportQualifiedPost #-}

module HelloAeson where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.KeyMap as KM
import Data.Text qualified as T

data Baz = Baz {bname :: String, orgId :: String}
  deriving (Show, Eq)

instance ToJSON Baz where
  toJSON Baz {..} =
    object
      [ "name" .= bname,
        "account"
          .= object
            [ "org_id" .= orgId
            ]
      ]

data Foo = Foo
  { age :: Int
  , name :: Maybe T.Text
  }
  deriving (Show, Eq)

instance FromJSON Foo where
  parseJSON = withObject "foo" $ \v -> Foo
    <$> v .: "age"
    <*> v `parseOptionalEmptyTextField` "name"

parseOptionalEmptyTextField :: FromJSON a => Object -> Key -> Parser (Maybe a)
parseOptionalEmptyTextField obj key = obj .:? key >>=  parseEmptyText
  where
    parseEmptyText :: FromJSON a => Maybe Value -> Parser (Maybe a)
    parseEmptyText Nothing = pure Nothing
    parseEmptyText (Just v) = withText
                          "parseOptionalEmptyTextField"
                          (\t -> if (T.null . T.strip) t then pure Nothing else Just <$> parseJSON v)
                          v

main = do
  print (encode $ Baz {bname = "hw", orgId = "org-123"})
  print (eitherDecode "{\"name\":\"Joe\",\"age\":12}" :: Either String Foo)
  print (eitherDecode "{\"name\":\"\",\"age\":12}" :: Either String Foo)
  print (eitherDecode "{\"name\":null,\"age\":12}" :: Either String Foo)
  print (eitherDecode "{\"age\":12}" :: Either String Foo)
