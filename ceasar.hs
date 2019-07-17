import Data.Char
import Data.List

let2int c = ord c - ord 'a'
int2let n = chr (ord 'a' + n)

shift n c | isLower c 	= int2let((let2int c + n) `mod` 26)
	  | otherwise 	= c

encode n xs = [shift n x | x <- xs ]

lowers xs = [ x | x <-xs, isLower x ]

percent :: Int -> Int -> Double
percent n m = (fromIntegral n / fromIntegral m) * 100

count x xs = length [ x' | x' <- xs, x == x']

freqs xs = [ percent (count x xs) n | x <- ['a'..'z']] 
	where n = length (lowers xs)

rotate n xs = drop n xs ++ take n xs

-- metric to compare to expected statistic
chisqr os es = sum [ ((o-e)^2) /e | (o,e) <- zip os es ]

-- expected statistic 
table :: [Double]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

chitab xs = [(n, chisqr (rotate n (freqs xs)) table) | n <- [0..25] ]

crack xs = [ encode (-factor) xs | (factor,_) <- topFactors ] 
		where 
			topFactors = take 5 (sortOn snd (chitab xs))
