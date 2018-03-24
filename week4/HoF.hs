{-# OPTIONS_GHC -Wall #-}

module HoF where

import Data.List ((\\))

main :: IO ()
main = do
  print $ xor [False, True, False] == True
  print $ xor [False, True, False, False, True] == False
  putStrLn ""

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . fmap (\x -> if even x then x - 2 else x)

fun2 :: Integer -> Integer
fun2 1        = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


-- data Tree a = Leaf
--             | Node Integer (Tree a) a (Tree a)
--             deriving (Show, Eq)

-- foldTree :: [a] -> Tree a
-- foldTree as = foldr intoTree Leaf as
--   where
--     intoTree :: a -> Tree a -> Tree a
--     intoTree a Leaf                     = Node 0 Leaf a Leaf
--     intoTree a (Node 0 Leaf val Leaf)   = Node 1 (intoTree a Leaf) val Leaf
--     intoTree a (Node 1 lTree val Leaf)  = Node 1 lTree val (intoTree a Leaf)
    -- intoTree a (Node height 
    --                  lTree@(Node lHeight _ _ _)
    --                  val 
    --                  rTree@(Node rHeight _ _ _)) = 
                       

xor :: [Bool] -> Bool
xor bs = odd . fst $ summary
  where
    count (trueCount, falseCount) True  = (trueCount + 1, falseCount)
    count (trueCount, falseCount) False = (trueCount, falseCount + 1)

    summary :: (Integer, Integer)
    summary = foldl count (0, 0) bs

map' :: (a -> b) -> [a] -> [b]
map' f = reverse . foldr doMapping []
  where 
    doMapping el acc = acc ++ [f el]


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = take (fromIntegral $ 2 * n + 2) . primesTo $ n
  where 
    primesTo m = sieve [2..n]
      where 
        sieve (x:xs) = x : sieve (xs \\ [x, x + x..m])
        sieve [] = []

