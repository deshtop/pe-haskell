import Data.Digits
import Data.List
import Euler

--listIt :: Integer -> [Integer]
--listIt n = digits 10 n

--unList :: [Integer] -> Integer
--unList n = unDigits 10 n

champ :: [Integer] -> [[Integer]]
champ [] = []
champ (n:ns) = (listIt n):(champ ns) 

concL :: [Integer]
concL = intercalate [] (champ [1..10^6])

prodL :: Integer
prodL = (concL !! 0)*(concL !! 9)*(concL !! 99)*(concL !! 999)*(concL !! 9999)*(concL !! 99999)*(concL !! 999999)

main = print $ prodL
