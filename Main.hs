import Data.Array as A
import Cube
import Solver

------- Merge solution moves -------
mergeSolutionMoves :: [Move] -> [Move]
mergeSolutionMoves [] = []
mergeSolutionMoves [move] = [move]
mergeSolutionMoves moves
    | head moves == ((head . drop 1) moves) = ((read . (++ "2") . show . head) moves) : (mergeSolutionMoves $ drop 2 moves) --- same move
    | head moves == oppositeMove ((head . drop 1) moves) = mergeSolutionMoves $ drop 2 moves --- opposite moves
    | (moveFunction . head) moves (((moveFunction . head . drop 1) moves) solved2x2) ==  --- make complement of moves
        (moveFunction . oppositeMove . head) moves solved2x2 = 
            ((oppositeMove . head) moves) : (mergeSolutionMoves $ drop 2 moves)
    | otherwise = (take 2 moves) ++ (mergeSolutionMoves $ drop 2 moves)


------- Solve wrappers ------
--- scramble by moves
solveWrapperScramble :: String -> [Move]
solveWrapperScramble scramble = (mergeSolutionMoves . solve) scrambledCube
    where scrambledCube = foldl (\cube move -> (moveFunction move) cube) solved2x2 moves
          moves = (map read . words) scramble

--- scrambled cube input
solveWrapperCube :: String -> [Move]
solveWrapperCube cube = (mergeSolutionMoves . solve) scrambledCube
    where scrambledCube = MakeCube $ A.array ((0, 0, 0), (1, 1, 1)) $ zip coords cubies
          coords = [(0, 1, 0), (0, 1, 1), (1, 1, 1), (1, 1, 0), 
                    (0, 0, 0), (0, 0, 1), (1, 0, 1), (1, 0, 0)]
          cubies = (splitEvery 3 . map read . words) cube :: [[Color]]

--- utility
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list


main = do
    putStrLn "Choose scramble option: move sequence (s) or cube colors (c): "  
    option <- getLine
    if (option == "s") then do
        putStrLn "Enter move sequence:"
        scramble <- getLine
        putStrLn "Solution:"
        putStrLn $ (unwords . map show) $ solveWrapperScramble scramble
    else if (option == "c") then do
        putStrLn "Enter cubie colors clockwise, starting with front left, layers top -> bottom, colors seq top/bottom, front/back, left/right:"
        colors <- getLine
        putStrLn "Solution:"
        putStrLn $ (unwords . map show) $ solveWrapperCube colors
    else do
        putStrLn "Invalid option."