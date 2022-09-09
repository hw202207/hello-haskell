{-# OPTIONS_GHC -Wpartial-fields #-}

-- |

module HelloPartialField where

data OptionRec a = None | Some { fromSome :: a }
  deriving Show

main :: IO ()
main = do
  let some = None :: OptionRec Int
  print (fromSome some)
  print (some { fromSome = 2 })
