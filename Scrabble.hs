{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty  = Score 0

score :: Char -> Score
score c
  | c `elem` "eaionrtlsu" = 1
  | c `elem` "dg" = 2
  | c `elem` "bcmp" = 3
  | c `elem` "fhvwy" = 4
  | c `elem` "k" = 5
  | c `elem` "jx" = 8
  | c `elem` "qz" = 10
  | otherwise = 0

scoreString :: String -> Score
scoreString = sum . map score
