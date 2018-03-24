{-# OPTIONS_GHC -Wall #-}

module Validate where

main :: IO ()
main = do
  print $ validate 4012888888881881
  print $ validate 4012888888881882

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0    = []
  | n <= 9    = [n] 
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse 
  where 
    doubleEveryOther' []         = []
    doubleEveryOther' [n]        = [n]
    doubleEveryOther' (n1:n2:ns) = n1 : 2 * n2 : doubleEveryOther' ns

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . fmap toDigits

validate :: Integer -> Bool
validate = (== 0) . (`rem` 10) . sumDigits . doubleEveryOther . toDigits

