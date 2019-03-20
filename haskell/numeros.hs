{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Numeros (myuntil, myfloor) where

import Debug.Trace

myuntil :: (a -> Bool) -> (a -> a) -> a -> a
myuntil p f x = if p x then x else myuntil p f (f x)


leq :: (Num a, Ord a) => Integer -> a -> Bool
leq m n = fromInteger m <= n

lt :: (Num a, Ord a) => a -> Integer -> Bool
lt  m n = m < fromInteger n

myfloor :: (Num a, Ord a) => a -> Integer
myfloor x = if x < 0
  then myuntil (`leq` x) (subtract 1) (-1)
  else myuntil (`lt` x) (+ 1) 0
  where leq m n = fromInteger m <= n
        lt  m n = fromInteger m < n


type Interval = (Integer, Integer)


myfloor2 :: Double -> Integer
myfloor2 x = fst (until unit (shrink x) (bound x))
             where unit (m, n) = (m + 1 == n)

shrink :: Double -> Interval -> Interval
shrink x (m, n) = if p `leq` x
                    then (p, n)
                    else (m, p)
                  where p = choose (m, n)


choose :: Interval -> Integer
choose (m, n) = (m + n) `div` 2


bound :: Double -> Interval
bound x = (lower x, upper x)

lower :: Double -> Integer
lower x = until (`leq` x) (*2) (-1)

upper :: Double -> Integer
upper x = until (x `lt`) (*2) 1


data Nat = Zero | Succ Nat

instance Eq Nat where
  Zero == Zero            = True
  Zero == Succ _          = False
  Succ _ == Zero          = False
  Succ m == Succ n        = (m == n)


instance Show Nat where
  show Zero = "Zero"
  show (Succ Zero) = "Succ Zero"
  show (Succ (Succ n)) = "Succ (" ++ show (Succ n) ++ ")"

instance Num Nat where
  m + Zero = m
  m + Succ n = Succ (m + n)

  _ * Zero = Zero
  m * (Succ n) = m * n + m

  abs n = n
  signum Zero = Zero
  signum (Succ _) = Succ Zero

  m - Zero = m
  Zero - Succ _ = Zero
  Succ m - Succ n = m - n
  fromInteger x
    | x <= 0    = Zero
    | otherwise = Succ (fromInteger (x - 1))
