import Data.Array as A
import Cube
import Solver
import Utils

------- Solve wrappers ------
--- scramble by moves
solveWrapperScramble :: String -> [Move]
solveWrapperScramble scramble = (mergeMoves . solve) scrambledCube
    where scrambledCube = foldl (\cube move -> (moveFunction move) cube) solved2x2 moves
          moves = (map read . words) scramble

--- scrambled cube input
solveWrapperCube :: String -> [Move]
solveWrapperCube cube = (mergeMoves . solve) scrambledCube
    where scrambledCube = MakeCube $ A.array ((0, 0, 0), (1, 1, 1)) $ zip coords cubies
          coords = [(0, 1, 0), (0, 1, 1), (1, 1, 1), (1, 1, 0), 
                    (0, 0, 0), (0, 0, 1), (1, 0, 1), (1, 0, 0)]
          cubies = (splitEvery 3 . map read . words) cube :: [[Color]]


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