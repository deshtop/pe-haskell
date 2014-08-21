import Euler

inc :: Integer -> Bool
inc n = incTest $ listIt n

incTest :: [Integer] -> Bool
incTest (n:m:ns)
    | n <= m    = incTest (m:ns)
    | otherwise = False
incTest (n:[]) = True

dec :: Integer -> Bool
dec n = decTest $ listIt n

decTest :: [Integer] -> Bool
decTest (n:m:ns)
    | n >= m    = decTest (m:ns)
    | otherwise = False
decTest (n:[]) = True

isBouncy :: Integer -> Bool
isBouncy n = (not $ inc n) && (not $ dec n)

count :: [Integer] -> Integer -> Integer
count (n:ns) acc
    | (isBouncy n) && (ratio >= 99)     = n
    | (isBouncy n)                      = count ns (acc+1)
    | otherwise                         = count ns acc
    where   ratio = (fromIntegral (acc+1)*100) / (fromIntegral n)

main = print $ count [1..] 0
