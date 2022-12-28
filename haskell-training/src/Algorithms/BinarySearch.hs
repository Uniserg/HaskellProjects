module Algorithms.BinarySearch (search, fromList) where

data SearchTree a = Node (SearchTree a) a (SearchTree a) | Nil deriving Show

fromList :: Int -> [a] -> SearchTree a
fromList _ [] = Nil
fromList n xs = Node (fromList d l) a (fromList (d - 1 + m) r) 
    where   
      (d, m) = n `divMod` 2
      (l, a:r)   = splitAt d xs

search :: (Integral a, Ord p) => a -> SearchTree p -> p -> a
search n t k = let
  f _ _ Nil = -1
  f a b (Node l x r)
    | x == k    = c + 1 
    | x >  k    = f a c l 
    | otherwise = f (c + 1) b r
    where 
      c = (a + b) `div` 2
  in f 0 n t