import Euler

revAdd :: Integer -> Integer
revAdd n = n + (unList $ reverse ln)
	where 	ln = listIt n

isPalindrome :: Integer -> Bool
isPalindrome n = ln == (reverse ln) 
	where ln = listIt n

testLychrel :: Int -> Integer -> Bool
testLychrel c n 
	| (c < 50) && (isPalindrome n)		= False
	| (c < 50) && (not $ isPalindrome n)	= testLychrel (c+1) (revAdd n) 
	| otherwise				= True

main = print $ length $ filter (==True) $ map (testLychrel 0) (map revAdd [1..9999])
