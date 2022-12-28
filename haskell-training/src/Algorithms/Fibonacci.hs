module Algorithms.Fibonacci (fib) where

import Data.List ( transpose )

newtype Matrix a = Matrix {getMatrix :: [[a]]} deriving (Show) 

fibM0 :: Matrix Integer
fibM0 = Matrix [
   [1, 1],
   [1, 0]
 ]

fib :: Int -> Integer
fib n = getMatrix (fibM0 |**| n) !! 0 !! 1

(|*|) :: Num a => Matrix a -> Matrix a -> Matrix a
(Matrix a) |*| (Matrix b) 
  | m /= n = error "Incompatible matrix"
  | otherwise = Matrix $ zipWith (\x _ -> zipWith (\x' y' -> sum (zipWith (*) x' y')) (replicate n x) tb) a tb
  where
    tb = transpose b
    m = length (head a)
    n = length b
    
(|**|) :: Num a => Matrix a -> Int -> Matrix a    
m |**| n
  | n == 1             = m
  | even n    = let m' = m |**| (n `div` 2) in m' |*| m'
  | otherwise = let m' = m |**| ((n - 1) `div` 2) in (m' |*| m') |*| m