{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Listas (myiterate, primerperfecto) where

primerperfecto :: Integer
primerperfecto = head (filter perfect [1..])
  where perfect n = (n == (sum (divisors n)))
        divisors x = [d | d <- [1..x-1], x `mod` d == 0]

myiterate :: (a -> a) -> a -> [a]
myiterate f x = x:myiterate f (f x)

-- [x * x | x <- [1..5]]
isqrt = floor . sqrt . fromIntegral
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x == 0]

-- λ> [x * x | x <- [1..5]]
-- [1,4,9,16,25]
-- λ> [x*x | x <- [1..5], isPrime x]
-- [1,4,9,25]
-- λ> [(i,j) | i <- [1..5], even i, j <- [i..5]]p
-- [(2,2),(2,3),(2,4),(2,5),(4,4),(4,5)]
-- λ> [x | xs <- [[(3,4)],[(5,4),(3,2)]], (3,x) <- xs]
-- [4,2]

triads :: Int -> [(Int,Int,Int)]
triads n = [(x,y,z) | x <- [1..n], y <- [1..n],
                      z <- [1..n], x*x + y*y == z*z]


disjoint :: Ord a => [a] -> [a] -> Bool
disjoint _ [] = True
disjoint [] _ = True
disjoint xs@(x:xt) ys@(y:yt)
  | x < y = disjoint xt ys
  | x == y = False
  | x > y = disjoint xs yt
