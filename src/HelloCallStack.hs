-- |

module HelloCallStack where

import GHC.Stack

foo :: HasCallStack => Int -> String
foo = error "oh no"

bar :: HasCallStack => Int -> String
bar = foo . negate

baz :: HasCallStack => Int -> String
baz = bar . (* 2)

main :: HasCallStack => IO ()
main = do
    print $ baz 5
