module HelloFactors where

-- | All possible factors of a number excluding itself
factorOf :: Integer -> [Integer]
factorOf n =
    -- filter (/= n) $
        concat $
            [[x, n `div` x] | x <- [1 .. sqrtIntegerVal n], n `rem` x == 0]
  where
    sqrtIntegerVal :: Integer -> Integer
    sqrtIntegerVal = floor . sqrt . fromIntegral

-- | Any number between 300-399 that has 12 factors?
run :: [(Integer, [Integer])]
run = [(x, factorOf x) | x <- [300 .. 399], length (factorOf x) == 24]

run2 :: [Integer]
run2 = [ x | x <- [300..399], isDivideByAll x]

isDivideByAll :: Integral a => a -> Bool
isDivideByAll n = all (== 0) [ n `rem` x | x<- [1..12] ]

main :: IO ()
main = do
    print $ show $ factorOf 21
    mapM_ print run
    mapM_ print run2
