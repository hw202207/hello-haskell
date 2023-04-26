{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HelloCryp where

import Control.Monad.IO.Class
import Crypto.Hash qualified as H
import Crypto.Random qualified as Crypto
import Data.ByteArray qualified as ByteArray
import Data.ByteString
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Gen

cv :: String
cv = "Lv6BZHaJDM9OZdCbraOnpufWUvxowHwu.IsG8NPlHMER_2WHvIfaqh4dcaphJmhS~OacldPaDCsfs2LVKBept0xG1jeS3TMFpBApAJ9etfRK-SFc.xmQrgag._leRNbS"

cc :: String
cc = "GJeXEWdiwiKVpCbJqjwJWwVmr2evwt1JSs2YZBHgWSs"

hashed :: ByteString
hashed = "189797116762c22295a426c9aa3c095b0566af67afc2dd494acd986411e0592b"

main :: IO ()
main = do
  putStrLn "--- test case 1 ---"
  str <- genCodeVerifier
  let hashed = hashSHA256 (BS8.pack str)
  let encoded = B64.encodeBase64Unpadded' . BS.pack . ByteArray.unpack . hashSHA256 . BS8.pack $ str
  putStrLn str
  print hashed
  print encoded
  putStrLn "--- test case 2 ---"
  str2 <- genCodeVerifier2
  let hashed2 = hashSHA256 str2
  let encoded2 = B64.encodeBase64Unpadded' . BS.pack . ByteArray.unpack . hashSHA256 $ str2
  print str2
  print hashed2
  print encoded2

hashSHA256 :: ByteString -> H.Digest H.SHA256
hashSHA256 = H.hash

genCodeVerifier :: (MonadIO m) => m String
genCodeVerifier =
  Gen.sample $
    Gen.list (Gen.singleton 128) $
      Gen.frequency [(10, Gen.alphaNum), (1, Gen.element ['-', '_', '.', '~'])]

{- | the expected characters are following.
The default 'getRandomBytes' generates bytes out of this scope.

@
[A-Z] / [a-z] / [0-9] / "-" / "." / "_" / "~"
@
-}
genCodeVerifier2 :: (MonadIO m) => m ByteString
genCodeVerifier2 = liftIO (getBytesInternal BS.empty)

getBytesInternal :: BS.ByteString -> IO BS.ByteString
getBytesInternal ba
  | BS.length ba >= 128 = pure (BS.take 128 ba)
  | otherwise = do
      bs <- Crypto.getRandomBytes 128
      let bsUnreserved = ba `BS.append` BS.filter isUnreversed bs
      getBytesInternal bsUnreserved

isUnreversed :: Word8 -> Bool
isUnreversed w = w `BS.elem` unreverseBS

unreverseBS :: BS.ByteString
unreverseBS = BS.pack $ [97 .. 122] ++ [65 .. 90] ++ [45, 46, 95, 126]
