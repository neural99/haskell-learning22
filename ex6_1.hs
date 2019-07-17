factorial 0 = 1
factorial (n+1) = (n+1) * factorial n

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y 	= x:y:ys
		| otherwise	= y: insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (n+2) = fibonacci n + fibonacci (n+1)

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger 
		where
		  smaller = [ a | a <- xs, a <= x ]
		  larger =  [ b | b <-xs, b > x ]

exp2 :: Integer -> Integer -> Integer
exp2 x 0 = 1
exp2 x (n+1) = x * exp2 x n

and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) = if x == True then and2 xs else False

concat2 [] = []
concat2 (x:xs) = x ++ concat2 xs

replicate2 0 x = []
replicate2 (n+1) x = x: replicate2 n x

select2 :: [a] -> Int -> a
select2 (x:xs) 0 = x
select2 (x:xs) (n+1) = select2 xs n 

elem2 :: Eq a => a -> [a] -> Bool
elem2 x [] = False
elem2 x (y:ys) = if x == y then True else elem2 x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then
				  x:merge xs (y:ys) 
				else
				  y:merge (x:xs) ys

halve :: Ord a => [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort f1) (msort f2) where (f1, f2) = halve xs
