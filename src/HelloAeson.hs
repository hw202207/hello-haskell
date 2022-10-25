{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HelloAeson where

import Data.Aeson

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

main = do
  print (encode $ Baz {bname = "hw", orgId = "org-123"})
