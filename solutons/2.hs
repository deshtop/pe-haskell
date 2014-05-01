fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

main = print $ sum [x | x <- take 50 fibs, x <= 4000000, even x]
