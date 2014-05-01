import Euler
import Data.Numbers.Primes
import Data.List

priF4 :: Integer -> Bool
priF4 n = (genericLength $ group $ primeFactors n) == 4

check :: Integer -> Integer 
check n
	| ((map (priF4) [n..n+3]) == (replicate 4 True))		= n
	| otherwise						= check (n+1)

main = print $ check 1
