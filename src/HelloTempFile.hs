-- |

module HelloTempFile where

import UnliftIO.Temporary
import System.IO

app1 :: IO ()
app1 = do
  bytes <- withSystemTempFile "hello-haskell.txt" $ \filePath handle1 -> do
    hClose handle1
    -- mimic file write
    withFile filePath WriteMode $ \handle2 -> do
      hPutStr handle2 "hello temp file 456"
      hClose handle2
    readFile filePath
  putStrLn bytes
