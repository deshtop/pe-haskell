largePrime :: Integer
largePrime = 28433 * 2^7830457 + 1

main = print $ reverse $ take 10 $ reverse $ show largePrime 
