{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List (sort)

main :: IO ()
main = do
  print $ localMaxima [2, 9, 5, 6, 1] == [9, 6]
  print $ localMaxima [2, 3, 4, 1, 5] == [4]
  print $ localMaxima [1, 2, 3, 4, 5] == []
  putStrLn ""

-- skips :: [a] -> [[a]]
-- skips []  = []
-- skips as  = 
--   where
--     indexed = zip [1..] as

localMaxima :: [Integer] -> [Integer]
localMaxima ns = fmap getCur . filter isLocalMaxima $ threes
  where 
    getCur :: (Integer, Integer, Integer) -> Integer
    getCur (_, cur, _) = cur

    isLocalMaxima :: (Integer, Integer, Integer) -> Bool
    isLocalMaxima (bef, cur, aft) 
      | bef <= cur && aft <= cur  = True
      | otherwise                 = False

    threes :: [(Integer, Integer, Integer)]
    threes = zip3 ns shiftRight1 shiftRight2
      where 
        shiftRight1 = tail ns
        shiftRight2 = tail shiftRight1

histogram :: [Integer] -> String
histogram ns =  -- | Hint: Transpose?
  where
    sorted :: [Integer]
    sorted = sort ns

    reduced :: [(Integer, Integer)]
    reduced = foldl summarize [] sorted
      where 
        summarize :: [(Integer, Integer)]
                  -> Integer
                  -> [(Integer, Integer)]
        summarize acc currNum =
          let (prevNum, prevCount) = last acc
           in if prevNum == currNum
                 then (init acc) ++ [(prevNum, prevCount + 1)]
                 else acc ++ [(currNum, 1)]

    maxCount :: Integer
    maxCount = foldl keepMax 0 reduced
      where 
        keepMax :: Integer
                -> (Integer, Integer)
                -> Integer
        keepMax acc (_, count) = max acc count

    yAxisData :: [[Integer]]
    yAxisData = 
      where
        init :: [[Integer]]
        init = repeat maxCount $ take 10 . repeat $ 0

    xAxisDraw :: [String]
    xAxisDraw = ["==========", "0123456789"]

