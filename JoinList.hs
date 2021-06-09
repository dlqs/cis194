{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Empty) = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ (Empty) = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append m jl1 jl2)
  | (Size i) < (size m) = indexJ i jl1
  | otherwise = indexJ i jl2

(!!?) :: [a] -> Int -> Maybe a
(!!?) (x:_) 0 = Just x
(!!?) xs i
  | null xs || i < 0 = Nothing
  | otherwise = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _) = Empty
dropJ i (Append m l1 l2)
  | (Size i) < (size $ tag $ l1) = dropJ i l1
  | otherwise = dropJ (i - (getSize $ size $ tag $ l1)) (Append m Empty l2)
  -- drop entire left subtree, and remaining amount in right subtree


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 jl = Empty
takeJ _ Empty = Empty
takeJ _ (Single _ _) = Empty
takeJ i (Append m l1 l2)
  | (Size i) < (size $ tag $ l1) = takeJ i l1
  | otherwise = Append m l1 (takeJ (i - (getSize $ size $ tag $ l1)) l2)
  -- take entire left subtree, and remaining difference from right subtree

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList(Score, Size) String) where
  toString     = unwords . jlToList
  fromString   s = Single (scoreString s, size s) s
  line n b     = safeIndex n (lines b)
  replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
      where replaceLine' pre [] = pre
            replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = length . lines
  value        = length . words
