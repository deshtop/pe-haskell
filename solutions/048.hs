import Euler
import Data.List

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (n:ns) = n^n + sum' ns 

prob48 :: Integer -> Integer
prob48 n = unList $ drop (len-10) $ listIt n
    where   ln = listIt n
            len = genericLength ln

main = print $ prob48 $ sum' [1..1000] 
 
