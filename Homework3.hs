module Golf where

import Data.List

skips :: [a] -> [[a]]

skips xs = map (\i -> drop i xs) [0..((length xs) - 1)]

localMaxima :: [Integer] -> [Integer]

localMaxima xs = map (\(y,_) -> y) $ filter (\(y,(x, z)) -> x < y && y > z) $ zip (drop 1 xs ) $ zip (drop 2 xs) (xs)


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
histogram xs = unlines [ (map (\i -> if i == -1 then ' ' else '*') r) | r <- createMatrix $ freqCount xs ] ++ ['=' | _ <- [0..9]] ++ "\n" ++ [ x | x <- ['0'..'9']] ++ "\n"
