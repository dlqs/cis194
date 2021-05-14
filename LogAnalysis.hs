{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage

parseMessage s =
  let split = words s in
  case (head split) of
    "I" -> LogMessage Info (read $ split!!1) (unwords . drop 2 $ split)
    "W" -> LogMessage Warning (read $ split!!1) (unwords . drop 2 $ split)
    "E" -> LogMessage (Error (read $ split!!1)) (read $ split!!2) (unwords . drop 3 $ split)

  -- a $ b c == a ( b c )

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"

insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) mt = mt
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node lmt (nlm@(LogMessage _ nts _)) rmt) 
  | ts < nts = Node (insert lm lmt) nlm rmt
  | ts > nts = Node lmt nlm (insert lm rmt)

inOrder :: MessageTree -> [LogMessage]

inOrder Leaf = []
inOrder (Node lmt lm rmt) = inOrder lmt ++ [lm] ++ inOrder rmt


whatWentWrong :: [LogMessage] -> [String]

whatWentWrong lms =
  let mt = 
  
