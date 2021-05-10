{-
stack ghci
:load Homework1.hs
-}

-- exercise 1
toDigits :: Integer -> [Integer]
toDigits a
  | a <= 0 = []
  | a <= 9 = [a]
  | otherwise = (toDigits (fst (divMod a 10)) ) ++ [a `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:t) = [x * 2, y] ++ doubleEveryOther t
doubleEveryOther (x:t) = [x * 2]

-- exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:t) = x + sumDigits t

-- exercise 4
validate :: Integer -> Bool
validate = ( == ) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits

-- exercise 5 hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a

