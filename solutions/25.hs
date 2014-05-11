import Data.Digits
import Data.List(elemIndex)

listIt :: Integer -> [Integer]
listIt n = digits 10 n

unList :: [Integer] -> Integer
unList n = unDigits 10 n

fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

countEm :: [Integer] -> Integer
countEm (f:fs)
	| (length $ listIt f) == 1000	= f
	| otherwise 			= countEm fs


main = print $ elemIndex (countEm fibs) fibs
