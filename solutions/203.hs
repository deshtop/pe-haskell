import Euler
import Data.Numbers.Primes
import Math.Combinatorics.Binomial
import Data.List

nums :: Integer -> [Integer]
nums n
	| n > 50	= []
	| otherwise	= (zipWith choose (replicate (fromIntegral (n+1)) n) [0..n])++(nums (n+1))

sqFree :: Integer -> Bool
sqFree n = nfcts == (nub $ nfcts)
	where nfcts = primeFactors n


main = print $ sum $ filter (sqFree) (nub $ nums 1)

