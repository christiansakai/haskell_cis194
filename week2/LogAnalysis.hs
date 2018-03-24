{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Control.Monad (forM_)

main :: IO ()
main = do
  print $ parseMessage "E 2 562 help help"
  print $ parseMessage "I 29 la la la"
  print $ parseMessage "This is not in the right format"
  putStrLn ""

  logMsgs <- testParse parse 10 "error.log"
  forM_ logMsgs print
  putStrLn ""

  -- Whoever did this must be some fantasy story fan.
  errorMsgs <- testWhatWentWrong parse whatWentWrong "error.log"
  forM_ errorMsgs print

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage str = 
  case words str of
    ("I":timeStamp:text) ->
      LogMessage Info (read timeStamp) (unwords text)

    ("W":timeStamp:text) ->
      LogMessage Warning (read timeStamp) (unwords text)

    ("E":errNum:timeStamp:text) ->
      LogMessage (Error (read errNum)) (read timeStamp) (unwords text)

    _ ->
      Unknown str

build :: [LogMessage] -> MessageTree
build []      = Leaf
build (l:ls)  = insert l (build ls)  

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree  = msgTree 
insert logMsg Leaf          = Node Leaf logMsg Leaf
insert logMsg (Node msgTreeL logMsg' msgTreeR) =
  let (LogMessage _ timeStmp _)  = logMsg
      (LogMessage _ timeStmp' _) = logMsg'
   in 
    if timeStmp < timeStmp' 
       then Node (insert logMsg msgTreeL) logMsg' msgTreeR
       else Node msgTreeL logMsg' (insert logMsg msgTreeR)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                            = []
inOrder (Node msgTreeL logMsg msgTreeR) =
  (inOrder msgTreeL) ++ [logMsg] ++ (inOrder msgTreeR)
        
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsgs = fmap getText errorAtLeast50
  where
    getText :: LogMessage -> String
    getText (LogMessage _ _ text) = text
    getText _                     = "Unknown"

    errorAtLeast50 :: [LogMessage]
    errorAtLeast50 = dropWhile notErrorBelow50 sortedLogMsgs

    sortedLogMsgs :: [LogMessage]
    sortedLogMsgs = inOrder (build logMsgs)

    notErrorBelow50 :: LogMessage -> Bool
    notErrorBelow50 (LogMessage Info _ _)     = False
    notErrorBelow50 (LogMessage Warning _ _)  = False
    notErrorBelow50 (LogMessage err _ _)      =
      let (Error severity) = err
       in if (severity < 50) then False else True
    notErrorBelow50 _                         = False

