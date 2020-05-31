{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cube where
import Data.Array as A
import ProblemState

--- util definitions
type Coords = (Int, Int, Int)

--- color definitions
data Color = White | Yellow | Blue | Green | Red | Orange deriving (Show, Eq, Read)

oppositeColor :: Color -> Color
oppositeColor White = Yellow
oppositeColor Yellow = White
oppositeColor Blue = Green
oppositeColor Green = Blue
oppositeColor Red = Orange
oppositeColor Orange = Red

--- cube definitions
type Cubie = [Color]
newtype Cube = MakeCube { getCubeArr :: A.Array (Int, Int, Int) Cubie } deriving (Show, Eq)

--- move definitions
data Move = R | R' | R2 | U | U' | U2 | F | F' | F2 deriving (Show, Eq, Read)

sameMoveType :: Move -> Move -> Bool
sameMoveType m1 m2
    | all (== True) $ map (`elem` [R, R', R2]) [m1, m2] = True
    | all (== True) $ map (`elem` [U, U', U2]) [m1, m2] = True
    | all (== True) $ map (`elem` [F, F', F2]) [m1, m2] = True
    | otherwise = False


oppositeMove :: Move -> Move
oppositeMove R = R'
oppositeMove R' = R
oppositeMove R2 = R2
oppositeMove U = U'
oppositeMove U' = U
oppositeMove U2 = U2
oppositeMove F = F'
oppositeMove F' = F
oppositeMove F2 = F2

moveFunction :: Move -> (Cube -> Cube)
moveFunction R = r
moveFunction R' = r'
moveFunction R2 = r2
moveFunction U = u
moveFunction U' = u'
moveFunction U2 = u2
moveFunction F = f
moveFunction F' = f'
moveFunction F2 = f2

--- merge moves of the same type in sequence
mergeMoves :: [Move] -> [Move]
mergeMoves [] = []
mergeMoves [move] = [move]
mergeMoves moves
    | head moves == ((head . drop 1) moves) = ((read . (++ "2") . show . head) moves) : (mergeMoves $ drop 2 moves) --- same move
    | head moves == oppositeMove ((head . drop 1) moves) = mergeMoves $ drop 2 moves --- opposite moves
    | (moveFunction . head) moves (((moveFunction . head . drop 1) moves) solved2x2) ==  --- make complement of moves
        (moveFunction . oppositeMove . head) moves solved2x2 = 
            ((oppositeMove . head) moves) : (mergeMoves $ drop 2 moves)
    | otherwise = (take 2 moves) ++ (mergeMoves $ drop 2 moves)

------ rotate cubie functions ------
rotateX :: (Coords, Cubie) -> (Int -> Int -> Bool) -> (Coords, Cubie)
rotateX ((x, y, z), [c1, c2, c3]) op = ((x, translatedY, translatedZ), [c2, c1, c3])
    where translatedY = if y `op` z then abs (1 - y) else y
          translatedZ = if not (y `op` z) then abs (1 - z) else z
rotateX (coords, cubie) _ = (coords, cubie)

rotateY :: (Coords, Cubie) -> (Int -> Int -> Bool) -> (Coords, Cubie)
rotateY ((x, y, z), [c1, c2, c3]) op = ((translatedX, y, translatedZ), [c1, c3, c2])
    where translatedZ = if x `op` z then abs (1 - z) else z
          translatedX = if not (x `op` z) then abs (1 - x) else x
rotateY (coords, cubie) _ = (coords, cubie)

rotateZ :: (Coords, Cubie) -> (Int -> Int -> Bool) -> (Coords, Cubie)
rotateZ ((x, y, z), [c1, c2, c3]) op = ((translatedX, translatedY, z), [c3, c2, c1])
    where translatedY = if x `op` y then abs (1 - y) else y
          translatedX = if not (x `op` y) then abs (1 - x) else x
rotateZ (coords, cubie) _ = (coords, cubie)


------ move functions ------
--- right moves
rAffectedCubies :: Cube -> [(Coords, Cubie)]
rAffectedCubies (MakeCube cubeArr) = [((1, y, z), cubie) | y <- [0, 1], z <-[0, 1], let cubie = cubeArr A.! (1, y, z)]

r :: Cube -> Cube
r cube@(MakeCube cubeArr) = MakeCube $ cubeArr A.// (map (`rotateX` (==)) $ rAffectedCubies cube)

r' :: Cube -> Cube
r' cube@(MakeCube cubeArr) = MakeCube $ cubeArr A.// (map (`rotateX` (/=)) $ rAffectedCubies cube)

r2 :: Cube -> Cube
r2 = r . r

--- up moves
uAffectedCubies :: Cube -> [(Coords, Cubie)]
uAffectedCubies (MakeCube cubeArr) = [((x, 1, z), cubie) | x <- [0, 1], z <-[0, 1], let cubie = cubeArr A.! (x, 1, z)]   

u :: Cube -> Cube
u cube@(MakeCube cubeArr) = MakeCube $ cubeArr A.// (map (`rotateY` (==)) $ uAffectedCubies cube)

u' :: Cube -> Cube
u' cube@(MakeCube cubeArr) = MakeCube $ cubeArr A.// (map (`rotateY` (/=)) $ uAffectedCubies cube)

u2 :: Cube -> Cube
u2 = u . u

--- front moves
fAffectedCubies :: Cube -> [(Coords, Cubie)]
fAffectedCubies (MakeCube cubeArr) = [((x, y, 0), cubie) | x <- [0, 1], y <-[0, 1], let cubie = cubeArr A.! (x, y, 0)]   

f :: Cube -> Cube
f cube@(MakeCube cubeArr) = MakeCube $ cubeArr A.// (map (`rotateZ` (==)) $ fAffectedCubies cube)

f' :: Cube -> Cube
f' cube@(MakeCube cubeArr) = MakeCube $ cubeArr A.// (map (`rotateZ` (/=)) $ fAffectedCubies cube)

f2 :: Cube -> Cube
f2 = f . f


------ solved ------
--- check solved
isSolved :: Cube -> Bool
isSolved (MakeCube cubeArr) = all (== True) [ head (cubeArr A.! (x, 1, y)) == topColor
                                              && cubeArr A.! (x, 1, y) == (map oppositeColor (cubeArr A.! (1 - x, 0, 1 - y)))
                                              | x <- [0, 1], y <- [0, 1]]
    where topColor = head (cubeArr A.! (0, 1, 0))

--- create solved for a given entry
createSolvedFor :: Cube -> Cube
createSolvedFor (MakeCube cubeArr) = MakeCube $ A.array ((0, 0, 0), (1, 1, 1)) [
                               ((0, 1, 0), [upColor, frontColor, leftColor]),
                               ((0, 1, 1), [upColor, backColor, leftColor]),
                               ((1, 1, 1), [upColor, backColor, rightColor]),
                               ((1, 1, 0), [upColor, frontColor, rightColor]),
                               ((0, 0, 0), [downColor, frontColor, leftColor]),
                               ((0, 0, 1), [downColor, backColor, leftColor]),
                               ((1, 0, 1), [downColor, backColor, rightColor]),
                               ((1, 0, 0), [downColor, frontColor, rightColor])]
    where [upColor, frontColor, rightColor] = map oppositeColor [downColor, backColor, leftColor]
          [downColor, backColor, leftColor] = cubeArr A.! (0, 0, 1)


------ instantiate ProblemState ------
instance ProblemState Cube Move where
    successors cube = [(R, r cube), (R', r' cube), (R2, r2 cube),
                       (U, u cube), (U', u' cube), (U2, u2 cube),
                       (F, f cube), (F', f' cube), (F2, f2 cube)]
    sameType = sameMoveType
    createFinal = createSolvedFor
    isGoal = isSolved
    reverseAction (move, cube) = ((oppositeMove move), (moveFunction (oppositeMove move)) cube)


--- Examples
--- white cube as canvas
whiteCube :: Cube
whiteCube = MakeCube $ A.array ((0, 0, 0), (1, 1, 1)) [((x, y, z), [White, White, White]) | x <- [0, 1], y <- [0, 1], z <- [0, 1]]

--- solved example
solved2x2 :: Cube
solved2x2 = createSolvedFor $ MakeCube $ (getCubeArr whiteCube) A.// [((0, 0, 1), [Yellow, Green, Red])]