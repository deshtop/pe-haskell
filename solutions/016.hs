import Data.Digits

listIt :: Integer -> [Integer]
listIt n = digits 10 n

prob16 :: Integer
prob16 = sum $ listIt $ 2^1000

main = print $ prob16
