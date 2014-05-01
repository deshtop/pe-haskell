isTB :: Integral a => a -> Bool
isTB a = a `mod` 3 == 0 || a `mod` 5 == 0

main = print $ sum [x | x <- [1..999], isTB x == True]

