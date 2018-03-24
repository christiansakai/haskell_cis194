{-# OPTIONS_GHC -Wall #-}

module Hanoi where

main :: IO ()
main = do
  print $ hanoi 2 "a" "b" "c"
  print $ hanoi' 4 "A" "D" "B" "C"

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _           = []
hanoi n peg1 peg2 peg3  = hanoi (n - 1) peg1 peg3 peg2 
                       ++ [(peg1, peg2)] 
                       ++ hanoi (n - 1) peg3 peg2 peg1

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 0 _ _ _ _             = []
hanoi' n peg1 peg2 peg3 peg4 = hanoi' (n - 2) peg1 peg3 peg4 peg2 
                            ++ [ (peg1, peg4)
                               , (peg1, peg2)
                               , (peg4, peg2)
                               ]
                            ++ hanoi' (n - 2) peg3 peg2 peg1 peg4
