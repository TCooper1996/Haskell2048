module Main where
import Data.List (transpose)
import Data.Maybe
--import System.Random
import Control.Applicative (Applicative(liftA2))

type Grid a = Rows a
type Tiles a = [Tile a]
type Pos = (Int, Int)

data Move = U | R | L | D
move2Rotations = [(R, 0), (D, 1), (L, 2), (U, 3)]

type Tile = Maybe
type Rows a = [Tiles a]
type Columns a = [Tiles a]

data GameState a = GameState {board :: Grid a, seed :: Int}

--instance Functor (GameState a) where
--    fmap f (b, s) = GameState $ f b s

--instance Applicative (GameState a) where
--    pure b = GameState $ b (makeStdGen 1000)


--instance Monad (GameState a) where
--    return = pure



getTile :: Grid a -> Pos -> Tile a
getTile g (x, y) = g !! y !! x


sizeOfGrid :: Grid a -> Int
sizeOfGrid rows = length rows * length (head rows)

replace :: [a] -> Int -> a -> [a]
replace lst ind newData = before ++ [newData] ++ after
    where (before,_:after) = splitAt ind lst

updateGrid :: Grid a -> Pos -> Tile a -> Grid a
updateGrid grid (x, y) newData = replace grid y newRow
    where oldRow = grid !! y
          newRow = replace oldRow x newData

--pos :: Tile a -> Pos
--pos (Tile x y _) = (x, y)

rows :: Grid a -> Rows a
rows = id

columns :: Grid a -> Columns a
columns = transpose

getEmptyTiles :: Grid a -> [Pos]
getEmptyTiles grid =
    let tileCount = sizeOfGrid grid
    in [pos |
       isNothing $ val $ getTile grid pos, 
       ind <- [0 .. tileCount],
       let pos = (tileCount `mod` ind, tileCount `div` ind)]

createBoard :: Int -> Grid a
createBoard = replicate <*> (`replicate` Tile Nothing)

shiftBoard :: Grid a -> Move -> Grid a
shiftBoard grid R = shiftBoardRight grid
shiftBoard grid m = rotate . shiftBoardRight . rotate grid
    where 
        rotations = lookup m Move2Rotations
        rotate g = iterate rotateBoard g !! rotations

rotateBoard :: Grid a -> Grid a
rotateBoard = transpose . map reverse

shiftBoardRight :: Grid a -> Grid a
shiftBoardRight = fmap shiftRowRight

shiftRowRight :: Tiles a -> Tiles a
shiftRowRight row = replicate emptyTiles Tile Nothing ++ tiles
    where size = length row
          (x:xs) = catMaybes row
          tiles = foldl collapse [x] xs
          collapse acc t
            | last acc == t = acc++[(*2) <$> t2]
            | otherwise = acc++[t1, t2]
          emptyTiles = size - length tiles

addNums :: Grid a -> Int -> Grid a
addNums grid seed = updateGrid grid pos (Just 2)
    where emptyTiles = getEmptyTiles grid
          newTile = seed `mod` sizeOfGrid grid
          len = length grid
          pos = (newTile `mod` len, newTile `div` len)

gBind :: GameState a -> (GameState a -> Int -> Grid a) -> GameState a
gBind gs@(g, seed) f = newGrid
    where newGrid = GameState $ f gs r newSeed
          (r, newSeed) = random seed


main :: IO ()
main = do
    let b = GameState $ createBoard $ makeStdGen 100
        newBoard = b `gBind` addNums `gBind` addNums
    play newBoard

play :: GameState a ->  IO ()
play grid 
    | emptyTiles grid == 1 = putStrLn "Game Over"
    | otherwise = do
    move <- getMove
    let 
        b = shiftBoard grid
        eTiles = emptyTiles b
        b2 = b `gBind` addNums 
    play b2

getMove :: IO Move
getMove = do
    putStrLn "Enter U, R, D, L"
    inp <- getChar 
    let out = parseMove inp
    if out == "0"
        then getMove
    else
        return out

    where 
    parseMove 'w' = U
    parseMove 'a' = L
    parseMove 's' = D
    parseMove 'd' = R
    parseMove _ = '0'
