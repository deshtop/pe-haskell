import Data.Digits

doublePal :: Int -> Bool
doublePal n = (l10n == reverse l10n) && (l2n == reverse l2n)
	where	l10n = digits 10 n
		l2n  = digits 2 n

main = print $ sum $ filter (doublePal) [1..1000000-1]

