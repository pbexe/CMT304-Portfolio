import Data.List

type Puzzle = [[Int]]
type Possible_Puzzles = [[[Int]]]

-- The input sudoku puzzle
input :: Puzzle
input = [[3,4,0,0],
         [2,0,3,0],
         [0,3,0,2],
         [0,0,1,3]]

-- Are there any repeat values?
is_set :: [Int] -> Bool
is_set [] = True
is_set (x:xs) = (if x == 0
                 then True
                 else not (x `elem` xs))
                 && is_set xs

-- Each row is valid
valid_rows :: Puzzle -> Bool
valid_rows x = all is_set x

-- Each col is valid
valid_cols :: Puzzle -> Bool
valid_cols x = valid_rows (transpose x)

-- Each quarter is valid
-- There has got to be a better way of doing this :'(
-- Should proabably use splitBlocks
valid_quarters :: Puzzle -> Bool
valid_quarters x = valid_rows [take 2 (x !! 0) ++ take 2 (x !! 1),
                               drop 2 (x !! 0) ++ drop 2 (x !! 1),
                               take 2 (x !! 2) ++ take 2 (x !! 3),
                               drop 2 (x !! 2) ++ drop 2 (x !! 3)]

-- The whole puzzle is valid 
valid_puzzle :: Puzzle -> Bool
valid_puzzle x = valid_rows x && valid_cols x && valid_quarters x

-- For a given square, what are the possibilities?
generate_choices :: Int -> [Int]
generate_choices x = if x == 0
                     then [1..4]
                     else [x]

-- For a given puzzle, what are all the possbilbilties for easch square?
all_possibilities :: Puzzle -> Possible_Puzzles
all_possibilities x = map (map generate_choices) x

-- Given all the possibilities, what are the possible puzzles?
all_puzzles :: Possible_Puzzles -> [Puzzle]
all_puzzles x = sequence (map sequence x)

-- And solve
solve :: Puzzle -> [Puzzle]
solve x = filter valid_puzzle (all_puzzles (all_possibilities x))

-- Run the program
main = print (head (solve input))
