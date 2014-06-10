import Data.Digits
import Euler

fSum :: Integer -> Integer
fSum n = sum $ map (fac) ln
	where ln = listIt n

p34 :: [Integer] -> Integer
p34 [] = 0
p34 (n:ns)
	| fSum n == n		= n + p34 ns
	| otherwise		= p34 ns

main = print $ p34 [3..10^7]
