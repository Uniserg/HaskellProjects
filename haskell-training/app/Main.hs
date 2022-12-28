module Main (main) where

-- import BinarySearch
import Algorithms.BinarySearch as Bs
import Algorithms.HuffmanEncoding as HuffmanEncoding
import Algorithms.HuffmanDecoding as HuffmanDecoding
import qualified Data.Map as Map
import Algorithms.Fibonacci as Fibonacci

main :: IO ()
main = do
    putStrLn $ "**************************** Бинарный поиск ****************************"
    putStrLn $ "Задача найти по второй строке индекс элемента в первой. Если нет, вывести -1."

    let s1 = "5 1 5 8 12 13" 
    let s2 = "5 8 1 23 1 11"

    let parse = map read . words
    let (n:a) = parse s1
    let (_:b) = parse s2

    let bst = Bs.fromList n a
    let searchN = Bs.search n bst
    putStrLn $ unwords $ map (show . searchN) b

    
    putStrLn $ "**************************** Кодирование по Хаффману ****************************"
    let s = "Hello world" 
    let (codeMap, encoded) = encodeHuffman s
    mapM_ (\(s, c) -> putStrLn $ s : ": " ++ c) (Map.toList codeMap)
    putStrLn $ encoded

    putStrLn $ "**************************** Декодирование по Хаффману ****************************"
    putStrLn $ HuffmanDecoding.decodeHuff (Map.toList codeMap) encoded

    putStrLn $ "**************************** Быстрый алгоритм на нахождение большого числа Финбоначчи через матрицу ****************************"
    print $ Fibonacci.fib 1000
