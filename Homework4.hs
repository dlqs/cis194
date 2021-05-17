-- |

module Homework4 where
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2)*fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> (x - 2) * acc) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' n = sum . filter even . takeWhile (>0) . iterate (\x -> if even x || x == 1 then x `div` 2 else 3*x + 1) $ n

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree xs = foldr insertTree Leaf xs
  where insertTree x Leaf = Node 0 Leaf x Leaf
        insertTree x (Node h lt n rt)
          | heightTree lt > heightTree rt = Node h lt n (insertTree x rt)
          | heightTree lt < heightTree rt = Node h (insertTree x lt) n rt
          | otherwise = let nlt = insertTree x lt in
                        Node (1 + max (heightTree nlt) (heightTree rt)) nlt n rt

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr (\x acc -> f acc x) z $ reverse xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (+1) . map(*2) . filter (\x -> not $ elem x [ i+j+2*i*j | i <- [1..n], j <- [1..n], i <= j]) $ [1..n]
--                                                                                                                 ^ this needed for compose to work
