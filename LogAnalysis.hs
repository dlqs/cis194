{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- ex 1
parseMessage :: String -> LogMessage

parseMessage s =
  let split = words s in
  case (head split) of
    "I" -> LogMessage Info (read $ split!!1) (unwords . drop 2 $ split)
    "W" -> LogMessage Warning (read $ split!!1) (unwords . drop 2 $ split)
    "E" -> LogMessage (Error (read $ split!!1)) (read $ split!!2) (unwords . drop 3 $ split)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"

-- ex 2
insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) mt = mt
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node lmt (nlm@(LogMessage _ nts _)) rmt) 
  | ts < nts = Node (insert lm lmt) nlm rmt
  | ts > nts = Node lmt nlm (insert lm rmt)

-- ex 3
build :: [LogMessage] -> MessageTree
build lms = helper lms Leaf
  where
    helper [] mt = mt
    helper (x:xs) mt = helper xs $ insert x mt

-- ex 4
inOrder :: MessageTree -> [LogMessage]

inOrder Leaf = []
inOrder (Node lmt lm rmt) = inOrder lmt ++ [lm] ++ inOrder rmt

-- ex 5
whatWentWrong :: [LogMessage] -> [String]

whatWentWrong lms =
  map messageString $ filter errorSeverityAtLeast50 $ inOrder $ build lms
  where
    errorSeverityAtLeast50 lm@(LogMessage (Error e) _ _)
      | e >= 50 = True
      | otherwise = False
    errorSeverityAtLeast50 _ = False
    messageString (LogMessage _ _ s) = s
    messageString _ = ""
