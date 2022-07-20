module Board where
import System.Random (StdGen, Random (random))
import Data.Maybe (catMaybes, isNothing)
import Data.List (transpose)
import Data.Function (on)
import GHC.Base (Alternative((<|>)))
import Data.Functor (($>))

data Grid = Grid {getGrid :: [[Tile]]}
type Tiles = [Tile]
type Pos = (Int, Int)
type Tile = Maybe Integer
type GState = (Grid, StdGen)

data Move = U | R | L | D
getRotation R  = 0
getRotation D  = 1
getRotation L  = 2
getRotation U  = 3

data RotationDirection = RLeft | RRight

makeGrid :: (Eq a, Num a) => [[a]] -> [[Maybe a]]
makeGrid g= fmap (\x -> if x==0 then Nothing else Just x) <$> g

--instance Functor Grid where
  --  fmap f (Grid g) = Grid $ fmap f <$> g

--instance Show (Grid Tile) where
  --  show = printBoard

instance Show Grid where
-- show :: Grid -> String
    show (Grid g) = concatMap ((++"\n") . printRow) g
        where
        largestNum = length . show . maximum $ catMaybes (concat g) <|> [1] -- Substitute dummy list if catMaybes is empty.
        printRow = unwords . map (padString . maybe "_" show)
        padding = flip subtract largestNum . length
        padString = (++) <$> id <*> (concat . flip replicate " " . padding)

sizeOfGrid :: Grid -> Int
sizeOfGrid = (2*).length . getGrid

replace :: [a] -> Int -> a -> [a]
replace lst ind newData = before ++ [newData] ++ after
    where (before,_:after) = splitAt ind lst

updateGrid :: Grid -> Pos -> Tile -> Grid
updateGrid (Grid grid) (x, y) newData = Grid $ replace grid y newRow
    where oldRow = grid !! y
          newRow = replace oldRow x newData



getEmptyTiles :: Grid -> [Pos]
getEmptyTiles (Grid grid) = fst <$> filteredGrid
    where
        numRows = length grid
        indexedGrid = zip [(x,y) | y <- [0..numRows-1], x <- [0..numRows-1]] $ concat grid
        filteredGrid = filter (isNothing . snd) indexedGrid


createBoard :: Int -> Grid
createBoard =  (Grid .) . replicate <*> (`replicate` Nothing)



shiftBoard :: Move -> Grid -> Grid
shiftBoard R = shiftBoardRight
shiftBoard m = rotateRight . shiftBoardRight . rotateLeft
    where
        rotations = getRotation m
        rotate dir g = iterate (rotateBoard dir) g !! rotations
        rotateLeft = rotate RLeft
        rotateRight = rotate RRight

rotateBoard :: RotationDirection -> Grid -> Grid
rotateBoard RRight = Grid . transpose . reverse . getGrid
rotateBoard RLeft = Grid . reverse . transpose . getGrid


shiftBoardRight :: Grid -> Grid
shiftBoardRight = Grid . fmap shiftRowRight . getGrid

{-shiftRowRight :: Tiles -> Tiles
shiftRowRight = (>>= (++)) <$> flip emptyTiles <*> getTiles
    where 
        getTiles = map Just <$> collapseRow . catMaybes
        emptyTiles =  (flip replicate Nothing .) . subtract `on` length
        -}

shiftRowRight :: Tiles -> Tiles
shiftRowRight row = emptyTiles ++ newTiles
    where
        newTiles = map Just <$> collapseRow . catMaybes $ row
        emptyTiles = flip replicate Nothing $ (subtract `on` length) newTiles row


collapseRow :: [Integer] -> [Integer]
collapseRow (a:b:cs)
    | a == b = a+b:collapseRow cs --With this line, [2,2,4] -> [0,4,4]
--  | a == b = a+b:collapseRow (b:cs) Use this line if a Tile can casecade multiple times in the same movement. eg, [2,2,4] -> [0, 0, 8]
    | otherwise = a:collapseRow (b:cs)
collapseRow row = row


addNums' :: Grid -> Int -> Grid
addNums' grid r = updateGrid grid newTile (pure 2)
    where emptyTiles = getEmptyTiles grid
          newTileInd = r `mod` length emptyTiles
          newTile = emptyTiles !! newTileInd



--addNums :: int -> GameState (GState (Grid Tile)) (Grid Tile)
addNums :: Int -> GState -> GState
addNums 0 gs = gs
addNums nums (grid, gen) = addNums (nums-1) $ 
  let (r, newGen) = random gen
      newBoard = addNums' grid r
                    in (newBoard, newGen)
