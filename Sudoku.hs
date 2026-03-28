module Sudoku where
rows = "ABCD"
cols = "1234"
containsElem :: Eq a => a-> [a]-> Bool
containsElem _ [] = False
containsElem elem (x:xs)
    | elem == x = True
    | otherwise = containsElem elem xs