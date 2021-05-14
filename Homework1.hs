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

-- first zip xs with indexes from the *right*. Then use the index to determine if double
doubleEveryOther xs =
  foldr (\(i, e) acc -> if odd i then 2*e:acc else e:acc) [] $ zip (reverse [0..(length xs)-1]) xs

-- exercise 3
sumDigits :: [Integer] -> Integer

-- use break each number into list of digits them sum those then sum those
sumDigits xs = foldr (\x s -> (+) s $ sum $ toDigits x) 0 xs

-- exercise 4
validate :: Integer -> Bool

-- lmao
validate = ( == ) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits

-- exercise 5 hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a
