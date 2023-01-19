module HelloScientific where

import Data.Scientific
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Scientific

a :: Scientific
a = 9876543210

main :: IO ()
main = print $ toLazyText (formatScientificBuilder Fixed (Just 0) a)
