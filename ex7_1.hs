import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w,b) <- zip weights bits ] where
			weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

all2 :: (a -> Bool) -> [a] -> Bool
all2 p = and . (map p)

any2 :: (a -> Bool) -> [a] -> Bool
any2 p = or . (map p)

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 p [] = []
takeWhile2 p (x:xs) | p x == True 	= x : takeWhile2 p xs
		    | otherwise		= []

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 p [] = []
dropWhile2 p (x:xs) | p x == True	= dropWhile2 p xs
		    | otherwise		= x:xs


factors n = [ i | i <-[1..n], n `mod` i == 0 ]
isPrime n = factors n == [1,n] 

primes = filter isPrime (iterate (+1) 1)

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0
