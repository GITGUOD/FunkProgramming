module Sudoku where
import Data.Char (digitToInt)
rows = "ABCD"
cols = "1234"
containsElem :: Eq a => a-> [a]-> Bool
containsElem _ [] = False
containsElem elem (x:xs)
    | elem == x = True
    | otherwise = containsElem elem xs

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x, y] | x <- xs, y <- ys]

replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\char -> if char == '.' then '0' else char)

-- Ger samma output som Cross
crossSquare :: String -> String -> [String]
crossSquare rows cols = [[row, col] | row <- rows, col <- cols]

matrix :: [[String]]
matrix = [[ [r,c] | c <- cols ] | r <- rows]

parseBoard :: String -> [(String, Int)]
parseBoard input = zip (crossSquare rows cols) (map digitToInt (replacePointsWithZeros input))