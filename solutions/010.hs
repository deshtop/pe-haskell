import Data.Numbers.Primes

p10 = sum $ filter (<2000000) (take 2000000 primes )

main = print $ p10
