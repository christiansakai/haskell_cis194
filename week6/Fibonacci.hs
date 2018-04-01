{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0 
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: Int -> [Integer]
fibs2 n = go 1 [0, 1]
  where
    go :: Int -> [Integer] -> [Integer]
    go index list 
      | index == n = list 
      | otherwise = 
          let a = list !! index 
              b = list !! (index - 1)
              compute = a + b
              listResult = list ++ [compute]
          in 
            go (index + 1) listResult

