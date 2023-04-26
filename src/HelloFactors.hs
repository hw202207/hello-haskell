module HelloFactors where

-- | All possible factors of a number excluding itself
factorOf :: Integer -> [Integer]
factorOf n =
    filter (/= n) $
        concat $
            [[x, n `div` x] | x <- [1 .. sqrtIntegerVal n], n `rem` x == 0]
  where
    sqrtIntegerVal :: Integer -> Integer
    sqrtIntegerVal = floor . sqrt . fromIntegral

-- | Any number between 300-399 that has 12 factors?
run :: [(Integer, [Integer])]
run = [(x, factorOf x) | x <- [300 .. 399], length (factorOf x) == 12]

main :: IO ()
main = do
    print $ show $ factorOf 21
    mapM_ print run
