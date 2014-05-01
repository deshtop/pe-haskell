module Euler 	(listIt
		,unList
		,fibs
		,fac
		,isPanDigi)
where

import Data.Digits
import Data.Numbers.Primes
import Data.List

listIt :: Integer -> [Integer]
listIt n = digits 10 n

unList :: [Integer] -> Integer
unList n = unDigits 10 n

fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

fac :: Integer -> Integer
fac n = product [1..n]

isPanDigi :: Integer -> Bool
isPanDigi n
	| ((sort $ take 9 ln) == [1..9]) && ((sort $ drop (len-9) ln) == [1..9])	= True
	| otherwise									= False
	where 	ln = listIt n
		len = genericLength ln
