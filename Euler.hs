module Euler 	
( listIt
, unList
, fibs
, fac
, isPanDigi )
where

import Data.Digits
import Data.Numbers.Primes
import Data.List

listIt :: (Integral a) => a -> [a]
listIt n = digits 10 n

unList :: (Integral a) => [a] -> a 
unList n = unDigits 10 n

fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

fac :: (Integral a) => a -> a 
fac n = product [1..n]

isPanDigi :: (Integral a) => a -> Bool
isPanDigi n = panDigiCheck (map fromIntegral $ sort ln) [1..len]
    where   ln = listIt n
            len = length ln

panDigiCheck :: [Int] -> [Int] -> Bool
panDigiCheck _ [] = True
panDigiCheck (n:ns) (l:ls)
	| n == l	    = panDigiCheck ns ls
	| otherwise 	= False
