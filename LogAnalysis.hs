{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- ex 1
parseMessage :: String -> LogMessage

-- "I"
parseMessage s =
  let split = words s in
  case (head split) of
    "I" -> LogMessage Info (read $ split!!1) (unwords . drop 2 $ split)
    "W" -> LogMessage Warning (read $ split!!1) (unwords . drop 2 $ split)
    "E" -> LogMessage (Error (read $ split!!1)) (read $ split!!2) (unwords . drop 3 $ split)
    _   -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"

-- ex 2
insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node lmt (nlm@(LogMessage _ nts _)) rmt) 
  | ts < nts = Node (insert lm lmt) nlm rmt
  | otherwise = Node lmt nlm rmt
--  | ts >= nts = Node lmt nlm (insert lm rmt) (apparently >= is not exhaustive here)

{- Shawn's
insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node lt cm@(LogMessage _ cts _) rt)
  | ts < cts = Node (insert m lt) cm rt
  | otherwise = Node lt cm (insert m rt)
insert (Unknown _) t = t
insert _ _ = error "passed wrong constructor"
 -}

-- ex 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

-- ex 4
inOrder :: MessageTree -> [LogMessage]

inOrder Leaf = []
inOrder (Node lmt lm rmt) = inOrder lmt ++ [lm] ++ inOrder rmt

-- ex 5
whatWentWrong :: [LogMessage] -> [String]

whatWentWrong lms =
  map messageString $ filter errorSeverityAtLeast50 $ inOrder $ build lms
  where
    errorSeverityAtLeast50 (LogMessage (Error e) _ _) = e >= 50
    errorSeverityAtLeast50 _ = False
    messageString (LogMessage _ _ s) = s
    messageString _ = ""
