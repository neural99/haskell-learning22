last2 xs = head (reverse xs)
last3 (x:xs) = if xs == [] then x else last3 xs 

init2 xs = take (length xs - 1) xs
init3 xs = reverse (tail (reverse xs))
