{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module HelloCryp where

import Control.Monad.IO.Class
import Crypto.Hash qualified as H
import Crypto.Random.Entropy
import Data.ByteString
import Data.ByteString.Base64.URL qualified as B64
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text.Lazy (Text, pack)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Gen
import Data.ByteArray qualified as ByteArray

cv :: String
cv = "Lv6BZHaJDM9OZdCbraOnpufWUvxowHwu.IsG8NPlHMER_2WHvIfaqh4dcaphJmhS~OacldPaDCsfs2LVKBept0xG1jeS3TMFpBApAJ9etfRK-SFc.xmQrgag._leRNbS"
cc = "GJeXEWdiwiKVpCbJqjwJWwVmr2evwt1JSs2YZBHgWSs"

hashed :: ByteString
hashed = "189797116762c22295a426c9aa3c095b0566af67afc2dd494acd986411e0592b"

main :: IO ()
main = do
  str <- genCodeVerifier
  let hashed = (hashSHA256 $ BS8.pack str)
  let encoded = base64URLEncode $ BS.pack $ ByteArray.unpack hashed
  putStrLn str
  print hashed
  print encoded

pkceCodeChallenge :: String -> ByteString
pkceCodeChallenge str =
  let hashed = (hashSHA256 $ BS8.pack str)
  in base64URLEncode $ BS.pack $ ByteArray.unpack hashed


-- hashSHA256 :: ByteString -> String
hashSHA256 bs = (H.hash bs :: H.Digest H.SHA256)

-- base64URLEncode :: ByteString -> ByteString
base64URLEncode =  B64.encodeBase64Unpadded'
  -- BS.takeWhile notEqSign . BS.map urlSafeConvert . B64.encode
  where
    urlSafeConvert :: Char -> Char
    urlSafeConvert '+' = '-'
    urlSafeConvert '/' = '_'
    urlSafeConvert x = x
    notEqSign :: Char -> Bool
    notEqSign = (/=) '='

genCodeVerifier :: MonadIO m => m String
genCodeVerifier =
  Gen.sample $
    Gen.list (Gen.singleton 128) $
      Gen.frequency [(10, Gen.alphaNum), (1, Gen.element ['-', '_', '.', '~'])]

genCodeVerifier2 :: IO ()
genCodeVerifier2 = do
  bs <- getEntropy 128
  BS.putStr (bs :: ByteString)
  print ""
  print (BS.length bs)
