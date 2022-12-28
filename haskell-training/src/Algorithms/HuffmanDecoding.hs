module Algorithms.HuffmanDecoding (decodeHuff) where

data DecodeTree = Fork DecodeTree DecodeTree | Leaf Char | Nil
type CodingMap = [(Char, String)]

buildDecodeTree :: CodingMap -> DecodeTree
buildDecodeTree m = let
  sub k = f where
      f ('0':vs) (Fork l r) = Fork (f vs l) r
      f ('1':vs) (Fork l r) = Fork l (f vs r)
      f ('0':vs) t'          = Fork (f vs (Leaf k)) t'
      f ('1':vs) t'          = Fork t' (f vs (Leaf k))
      f _        _          = Leaf k

  build []          t = t
  build ((k, v):ms) t = build ms (sub k v t)
  in
    build m Nil


decodeHuff :: CodingMap -> String -> String
decodeHuff m s = let
  huffTree = buildDecodeTree m

  decode ('0':xs) (Fork l _) = decode xs l
  decode ('1':xs) (Fork _ r) = decode xs r
  decode xs       (Leaf x)  = x:decode xs huffTree
  decode _        _          = []
  in
    decode s huffTree