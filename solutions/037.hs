import Euler
import Data.Numbers.Primes

check :: [Integer] -> Int -> Integer
check _ 11 = 0
check (p:ps) c
	| isTrunc p		= p + check ps (c+1)
	| otherwise		= check ps c

isTrunc :: Integer -> Bool
isTrunc n = (isTruncL ln) && (isTruncR ln)
	where ln = listIt n

isTruncL :: [Integer] -> Bool
isTruncL [] = True
isTruncL (p:ps)
	| isPrime $ unList (p:ps)	= isTruncL ps
	| otherwise			= False

isTruncR :: [Integer] -> Bool
isTruncR [] = True
isTruncR ps
        | isPrime $ unList ps		= isTruncR (init ps)
        | otherwise                     = False

main = print $ check (drop 4 primes) 0
