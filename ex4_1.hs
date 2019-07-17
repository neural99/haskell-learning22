isDigit2 :: Char -> Bool
isDigit2 c = c >= '0' && c <= '9'

isEven2 n = n `mod`2 == 0

sign2 n | n < 0 	= -1
		| n == 0 	= 0
		| otherwise = 1

odds n = map (\x -> x*2+1)[0.. n-1]

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) 
				where
		 			n = (length xs)`div`2

safetail1 xs = if xs == [] then [] else tail xs

safetail2 xs | xs == [] 	= []
						 | otherwise  = tail xs

safetail3 [] = []
safetail3 (x:xs) = xs

and2 x y = if x == True && y == True then True else False
and3 x y = if x == True then y else False
