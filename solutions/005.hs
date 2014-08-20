
evenDiv :: Integer -> Integer -> Bool
evenDiv n d
    | d > 20            = True
    | (mod n d) /= 0    = False
    | otherwise         = evenDiv n (d+1)

prob5 :: Integer -> Integer 
prob5 n
    | (evenDiv n 2) == True     = n
    | otherwise                 = prob5 (n+1)

main = print $ prob5 1
