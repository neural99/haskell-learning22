imgSize :: Image -> Int 
imgSize img = (floor . sqrt . fromIntegral . length) img

colorAt :: Image -> Int -> Int -> Int
colorAt img x y = img !! (x + y * imgSize img)

type Image = [ Int ]
type Mask = [ Bool ]

maskOr1:: Mask -> Mask -> Mask
maskOr1 m1 m2 = zipWith (||) m1 m2

maskOr :: [Mask ] -> Mask
maskOr lst = foldr maskOr1 tmask lst where
		tmask = take ((length . head) lst) (repeat False)

updateMask :: Image ->Mask -> Int -> Int -> Mask
updateMask img mask x y = let ind = x + y * imgSize img in 
			take ind mask ++ True : drop (ind + 1) mask

incHist :: [ Int ] -> Int -> [Int]
incHist hist col = let val = hist !! col in
			 take col hist ++ (val + 1) : drop (col + 1) hist where

floodFill :: Image -> Mask -> Int -> Int -> Int -> Mask
floodFill img mask col x y  
		| x < 0 = mask
		| x >= (imgSize img) = mask
		| y < 0 = mask
		| y >= (imgSize img) = mask
		| (mask !! (x + y * imgSize img)) = mask
		| (colorAt img x y) /= col = mask
		| otherwise = maskOr [ south, north, west, east ] where
			nmask = updateMask img mask x y
			south = floodFill img nmask col x (y+1) 
			north = floodFill img nmask col x (y-1) 
			west = floodFill img nmask col (x-1) y
			east = floodFill img nmask col (x+1) y

emptyMask :: Image -> Mask
emptyMask img = take (imgSize img * imgSize img) (repeat False)

findRegion :: Image -> Int -> Int -> Mask
findRegion img x y = floodFill img (emptyMask img) (colorAt img x y) x y

buildHistogram :: Image -> Mask -> [Int] -> Int -> Int -> (Mask, [Int])
buildHistogram img visited hist x y 
	| x >= imgSize img = buildHistogram img visited hist 0 (y + 1)
	| y >= imgSize img = (visited, hist)
	| visited !! (x + y * imgSize img) = buildHistogram img visited hist (x + 1) y 
	| otherwise = buildHistogram img (maskOr1 visited (findRegion img x y)) newHist x y 
		where 
			col = colorAt img x y
			newHist = incHist hist col

histogram :: Image -> [ Int ]
histogram img = hist where
		(_, hist) = buildHistogram img emptyMask emptyHist 0 0 where
				emptyMask = take (imgSize img * imgSize img) (repeat False)
				emptyHist = take 256 (repeat 0)

main :: IO()
main = do 
	input <- getLine
	let lst = read input :: [Int]
	print (histogram lst)
