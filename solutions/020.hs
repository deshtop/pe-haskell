import Data.Digits

listIt :: Integer -> [Integer]
listIt n = digits 10 n

prob20 :: Integer
prob20 = sum $ listIt $ fac 100

fac :: Integer -> Integer
fac n = product [1..n]

main = print $ prob20
