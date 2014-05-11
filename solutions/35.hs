import Euler
import Data.List
import Data.Numbers.Primes

rotations :: Integer -> [Integer]
rotations n = take len $ iterate rotate n
	where 	len = genericLength ln
		ln = listIt n
		

rotate :: Integer -> Integer
rotate n = unList $ (last ln):(init ln)
	where ln = listIt n

arePrime :: [Integer] -> Bool
arePrime [] = True
arePrime (x:xs) 
	| isPrime x	= arePrime xs
	| otherwise	= False

check :: Integer -> [Integer]
check 1000000 = []
check n
	| arePrime $ rotations  n	= n:check (n+1)
	| otherwise			= check (n+1)

main = print $ length $ check 1
