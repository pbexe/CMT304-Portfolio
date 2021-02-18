import Data.List

type Board = [[Int]]

-- The input sudoku puzzle
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
-- There has got to be a better way of doing this :'(
-- Should proabably use splitBlocks
valid_quarters :: Board -> Bool
valid_quarters x = valid_rows [take 2 (x !! 0) ++ take 2 (x !! 1),
                               drop 2 (x !! 0) ++ drop 2 (x !! 1),
                               take 2 (x !! 2) ++ take 2 (x !! 3),
                               drop 2 (x !! 2) ++ drop 2 (x !! 3)]

-- The whole board is valid 
valid_board :: Board -> Bool
valid_board x = valid_rows x && valid_cols x && valid_quarters x

-- For a given square, what are the possibilities?
generate_choices :: Int -> [Int]
generate_choices x = if x == 0
                     then [1..4]
                     else [x]

-- For a given board, what are all the possbilbilties for easch square?
all_possibilities :: Board -> [[[Int]]]
all_possibilities x = map (map generate_choices) x

-- Given all the possibilities, what are the possible boards?
all_boards :: [[[Int]]] -> [Board]
all_boards x = sequence (map sequence x)

-- And solve
solve :: Board -> [Board]
solve x = filter valid_board (all_boards (all_possibilities x))

main = print (head (solve input))