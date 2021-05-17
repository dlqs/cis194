module Golf where

import Data.List

skips :: [a] -> [[a]]

skips xs = map (\i -> [ e | (e, j) <- zip xs [1..], mod j i == 0]) [1..(length xs)]

localMaxima :: [Integer] -> [Integer]

localMaxima xs = map (\(y, _, _) -> y) $ filter (\(y, x, z) -> x < y && y > z) $ zip3 (drop 1 xs) (drop 2 xs) (xs)


-- left pads a list with -1's
-- leftPad 3 [1, 2] = [-1, 1, 2]
leftPad :: Int -> [Integer] -> [Integer]
--leftPad l xs = [-1 | _ <- l - (length xs)] ++ xs
leftPad l xs
  | length xs >= l = xs
  | otherwise = [-1] ++ leftPad (l - 1) xs

-- freqCount [1, 3, 3] = [(1, 1), (3, 2)]
freqCount :: [Integer] -> [(Integer, Int)]
--freqCount xs = map (\x -> (head x, length x)) $ group $ sort xs
freqCount xs = foldr (\x acc -> [ if e == x then (e, f + 1) else (e, f) | (e, f) <- acc]) [(x, 0) | x <- [0..9]] xs

maxFreq :: [(Integer, Int)] -> Int
maxFreq = maximum . map snd

createMatrix :: [(Integer, Int)] -> [[Integer]]
createMatrix xs =
  let m = maxFreq xs in
  transpose [ leftPad m [e | x <- [0..f - 1]] | (e, f) <- xs ]

histogram :: [Integer] -> String
histogram xs = unlines [ [if i == -1 then ' ' else '*' | i <- r ] | r <- createMatrix $ freqCount xs ] ++ ['=' | _ <- [0..9]] ++ "\n" ++ [ x | x <- ['0'..'9']] ++ "\n"
