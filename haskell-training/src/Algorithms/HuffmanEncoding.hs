module  Algorithms.HuffmanEncoding (encodeHuffman, EncodeMap) where

import qualified Data.Map as Map


data BiTree a = Node {
  rank :: Int,
  root :: a,
  children :: [BiTree a]}

type BiHeap a = [BiTree a]

link :: Ord a => BiTree a -> BiTree a -> BiTree a
link t1@(Node r x c1) t2@(Node _ y c2)
  | x < y     = Node (r + 1) x (t2:c1)
  | otherwise = Node (r + 1) y (t1 : c2)

insertTree :: Ord a => BiHeap a -> BiTree a -> [BiTree a]
insertTree []          t = [t]
insertTree ts@(t':ts') t
  | rank t < rank t'     = t:ts
  | otherwise            = insertTree ts' (link t t')


insert :: Ord a => BiHeap a -> a -> BiHeap a
insert h x = insertTree h (Node 0 x [])

fromList :: Ord a => [a] -> BiHeap a
fromList = foldl insert []

merge :: Ord a => BiHeap a -> BiHeap a -> BiHeap a
merge h1            [] = h1
merge []            h2 = h2
merge ts1@(t1:ts1') ts2@(t2:ts2')
  | rank t1 < rank t2  = t1:merge ts1' ts2
  | rank t1 > rank t2  = t2:merge ts1 ts2'
  | otherwise          = insertTree (merge ts1' ts2') (link t1 t2)


extractMin :: Ord a => BiHeap a -> (a, BiHeap a)
extractMin h = (root t, merge (reverse (children t)) h')
  where  
    extract :: Ord a => BiHeap a -> (BiTree a, BiHeap a)
    extract []           = error "Empty heap."
    extract [x]          = (x, [])
    extract (x:xs)
      | root x < root t' = (x, xs)
      | otherwise        = (t', x:ts')
      where (t', ts') = extract xs

    (t, h') = extract h

newtype LetterFreq = LetterFreq {getLetterFreq :: (Char, Int)}

data HuffmanCodeTree = Branch HuffmanCodeTree Int HuffmanCodeTree | Leaf LetterFreq

getKey :: HuffmanCodeTree -> Int
getKey (Leaf x)       = snd . getLetterFreq $ x
getKey (Branch _ x _) = x

instance Eq HuffmanCodeTree where
  a == b = getKey a == getKey b

instance Ord HuffmanCodeTree where
  a <= b = getKey a <= getKey b

buildTree :: BiHeap HuffmanCodeTree -> HuffmanCodeTree
buildTree [t] | rank t == 0 = root t
buildTree h = let
  (x, h')  = extractMin h
  (y, h'') = extractMin h'
  in buildTree $ insert h'' (Branch x (getKey x + getKey y) y)


type EncodeMap = Map.Map Char String

getEncodeMap :: HuffmanCodeTree -> EncodeMap
getEncodeMap (Leaf a) = Map.insert (fst . getLetterFreq $ a) "0" Map.empty
getEncodeMap t = let
  encodeSyms (Leaf a)       s m = Map.insert (fst . getLetterFreq $ a) s m 
  encodeSyms (Branch l _ r) s m = Map.union (encodeSyms l (s ++ "0") m) (encodeSyms r (s ++ "1") m)
  in
    encodeSyms t "" Map.empty


encode :: String -> EncodeMap -> String
encode ""     _ = ""
encode (x:xs) m = (m Map.! x) ++ encode xs m

encodeHuffman :: String -> (EncodeMap, String)
encodeHuffman ""     = (Map.empty, "")
encodeHuffman s = let
  freq = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- s]
  heap = fromList $ map (Leaf . LetterFreq) freq
  huffmanCodeTree = buildTree heap
  encodeMap = getEncodeMap huffmanCodeTree
  encoded = encode s (getEncodeMap huffmanCodeTree)
  in
    (encodeMap, encoded)