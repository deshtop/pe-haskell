import Euler


powerSum :: [Integer] -> Integer 
powerSum ln = sum $ map (^5) ln

iterateNrs :: Integer -> [Integer]
iterateNrs n
    | n > 200000        = []
    | pwrSum == n       = pwrSum : iterateNrs (n+1)
    | otherwise         = iterateNrs (n+1) 
    where   ln = listIt n
            pwrSum = powerSum ln
