-- |

module HelloTime where

import Data.Time.Clock
import Data.Time.Format

b :: IO UTCTime
b = parseTimeM True defaultTimeLocale "%FT%T" "2020-05-27T09:24:59"

c :: String -> String -> IO UTCTime
c = parseTimeM True defaultTimeLocale

main :: IO ()
main = do
  b >>= print
