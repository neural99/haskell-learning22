factors n = [ x | x <-[1..n], n `mod` x == 0]

prime 1 = True
prime n = factors n == [1,n]

primes n = [x | x <- [2..n], prime x]

sumsqr n = sum [x^2 | x <- [1..n] ]

replicate2 n x = [ x | k<-[1..n]]

pyths n = [ (x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2==z^2 ]

isPerfect n = sum (factors n) == 2*n

perfects n = [ x | x <- [1..n], isPerfect x]

scalarprod xs ys = sum [ x*y | (x,y) <- zip xs ys ]
