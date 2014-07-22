import Euler
import Data.List

maxFrac :: Float 
maxFrac = 10/6

prob52 :: Integer -> Integer
prob52 n
    | n >= maxDigits            = maxDigits
    | testNr n (sort ln) 2      = n
    | otherwise                 = prob52 $ n + 1
    where   ln = listIt n
            len = genericLength ln
            maxDigits = ceiling maxFrac*10^len

testNr :: Integer -> [Integer] -> Integer -> Bool
testNr _ _ 7    = True
testNr n sln run
    | sln == (sort $ listIt $ n*run)    = testNr n sln (run+1)
    | otherwise                         = False

main = print $ prob52 1
