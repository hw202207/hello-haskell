{-# LANGUAGE TypeApplications #-}
module HelloPartialMatching where

data Status = InProgress Int String | Done String
    deriving (Show, Eq, Ord)

getStatus :: MonadFail m => Int -> m String
getStatus 0 = pure "zero"
getStatus 1 = fail "false"
getStatus _ = fail "true"

main :: IO ()
main = do
    let (Just x) = getStatus @Maybe 4
    print x
