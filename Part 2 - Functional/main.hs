import Data.List

type Board = [[Int]]

input :: Board
input = [[3,4,0,0],
         [2,0,3,0],
         [0,3,0,2],
         [0,0,1,3]]

-- If each row, column, and 2x2 block `is_set` then the puzzle is valid
is_set :: [Int] -> Bool
is_set [] = True
is_set (x:xs) = (if x == 0
                 then True
                 else not (x `elem` xs))
                 && is_set xs

-- Each row is valid
valid_rows :: Board -> Bool
valid_rows x = all is_set x

-- Each col is valid
valid_cols :: Board -> Bool
valid_cols x = valid_rows (transpose x)

-- Each quarter is valid
-- There has got to be a better way of doing this
valid_quarters :: Board -> Bool
valid_quarters x = valid_rows [take 2 (x !! 0) ++ take 2 (x !! 1),
                    drop 2 (x !! 0) ++ drop 2 (x !! 1),
                    take 2 (x !! 2) ++ take 2 (x !! 3),
                    drop 2 (x !! 2) ++ drop 2 (x !! 3)]

-- The whole board is valid
valid_board :: Board -> Bool
valid_board x = valid_rows x && valid_cols x

-- There are no 0s remaining
board_finished :: Board -> Bool
board_finished x = notElem True (map (elem 0) x)