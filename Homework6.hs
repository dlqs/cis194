-- |

module Homework6 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [ fib n | n <- [0..] ]

-- fibs2 :: [Integer]

data Stream a = Stream a

streamToList :: Stream a -> [a]
streamToList s@(Stream a) = a : streamToList s

instance Show a => Show (Stream a) where
  show xs = show $ take 20 (streamToList xs)

streamRepeat :: a -> Stream a
streamRepeat x = Stream x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a) = Stream (f a)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed =
