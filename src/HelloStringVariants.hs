{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HelloStringVariants where

import Data.Aeson
import Data.StringVariants
import GHC.Generics
import Text.Pretty.Simple
import qualified Data.List.NonEmpty  as NE

{-
instance KnownNat n => FromJSON (NullableNonEmptyText n) where
  parseJSON = \case
    J.String t -> case mkNullableNonEmptyText t of
      Just txt -> pure txt
      Nothing -> fail $ "Data/StringVariants/NullableNonEmptyText.hs: When trying to parse a NullableNonEmptyText, expected a String of length < " ++ show (natVal (Proxy @n)) ++ ", but received: " ++ T.unpack t
    J.Null -> pure $ NullableNonEmptyText Nothing
    x -> fail $ "Data/StringVariants/NullableNonEmptyText.hs: When trying to parse a NullableNonEmptyText, expected a String or Null, but received: " ++ show x
-}

{-
Why 'address' can not be omitted?

Aeson handles Maybe differently than others.
Think of Aeson is smart enough to invoke one of following methods depending on the type (when auto generates To/FromJSON instances)
Apparently, use '(.:?)' for Maybe type

(.:) :: (FromJSON a) => Object -> Key -> Parser a
(.:?) :: (FromJSON a) => Object -> Key -> Parser (Maybe a)

-}

data Foo = Foo
    { name :: NonEmptyText 10
    , name2 :: Maybe (NonEmptyText 10)
    , address :: NullableNonEmptyText 10
    , address2 :: Maybe (NullableNonEmptyText 10)
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON Foo


txtChunks :: NonEmptyText 140 -> [Maybe ( NonEmptyText 35 )]
txtChunks =
      NE.take 4
        . (flip NE.appendList $ repeat Nothing)
        . NE.map Just
        . chunksOfNonEmptyText @35 @140

bar :: Maybe (NonEmptyText 140)
bar = mkNonEmptyTextWithTruncate "RETURN IMAD 20000312MMQFMP4S000011"

main :: IO ()
main = do
    let xs =
            [ "{\"name\": \"foo\", \"address\": \"\"}"
            , "{\"name\": \"foo\", \"address\": \"test\"}"
            , "{\"name\": \"foo\"}"
            ]
    mapM_ (pPrint . eitherDecode @Foo) xs
    print (txtChunks <$> bar)
