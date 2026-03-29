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


-- Assignment 2:

-- Task 1
rowUnits :: [[String]]
rowUnits = [[r:c:[] | c <- cols] | r <- rows]

colUnits :: [[String]]
colUnits = [[r:c:[] | r <- rows] | c <- cols]

boxUnits :: [[String]]
boxUnits = [cross rs cs | rs <- ["AB","CD"], cs <- ["12","34"]]

unitList :: [[String]]
unitList = rowUnits ++ colUnits ++ boxUnits

-- Task 2:

filterUnitList :: String -> [[String]]
filterUnitList square = [unit | unit <- unitList, containsElem square unit]

-- Task 3:
units :: [(String, [[String]])]
units = [(sq, filterUnitList sq) | sq <- crossSquare rows cols]

-- Task 4:

foldList :: [[a]] -> [a]
foldList = concat


-- Task 5:
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (first:last)
    | first `elem` last = removeDuplicates last
    | otherwise   = first : removeDuplicates last

-- Task 6

peers :: [(String, [String])]
peers = [(sq, removeDuplicates (filter (/= sq) (foldList (filterUnitList sq)))) | sq <- crossSquare rows cols]