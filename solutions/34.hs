import Data.Digits

listIt :: Integer -> [Integer]
listIt n = digits 10 n

fac :: Integer -> Integer
fac n = product [1..n]


fSum :: Integer -> Integer
fSum n = sum $ map (fac) ln
	where ln = listIt n

p34 :: [Integer] -> Integer
p34 [] = 0
p34 (n:ns)
	| fSum n == n		= n + p34 ns
	| otherwise		= p34 ns

main = print $ p34 [3..10^7]
