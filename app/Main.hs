{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Data.List (transpose, intersperse)
import Data.Function(on)
import Data.Maybe
import System.Random

import Control.Applicative (Applicative(liftA2), Alternative ((<|>)))
import Control.Monad.State (State, StateT (StateT), state, MonadTrans, MonadState)
import Data.Bits (Bits(shift))
import Control.Monad (ap, liftM)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.RWS (MonadState(get, put))

data Grid a = Grid {getGrid :: [[a]]}
type Tiles = [Tile]
type Pos = (Int, Int)

data Move = U | R | L | D
getRotation R  = 0
getRotation D  = 1
getRotation L  = 2
getRotation U  = 3

data RotationDirection = RLeft | RRight

type Tile = Maybe Integer
type Rows = [Tiles]
type Columns = [Tiles]

--data GState a = GState {getGrid1 :: Grid a, getGen ::  StdGen}
data GState a = GState {getVal :: a, getGen ::  StdGen}
instance Functor GState where
    fmap f (GState g gen) = GState (f g) gen

--newtype GameState s = GameState {getState :: s}
newtype GameState s a = GameState {runState :: s -> (a, s)}

newtype StateIO s m a= StateIO {runStateT :: (s -> m (a,s))}

instance (Monad m) => Functor (StateIO s m) where
    fmap = liftM

instance (Monad m) => Applicative (StateIO s m) where
    pure = return
    (<*>) = ap

instance (Monad m) => MonadState s (StateIO s m) where
    get = StateIO $ \s -> return (s,s)
    put s = StateIO $ \_ -> return ((),s)


instance (Monad m) => Monad (StateIO s m) where
    return x = StateIO $ \s -> return (x, s)
    (StateIO x) >>= f = StateIO $ 
     \s -> do 
        (out, s2) <- x s
        (StateIO x') <- return $ f out
        x' s2

instance MonadTrans (StateIO s) where
    lift c = StateIO $ \s -> c >>= (\x -> return (x,s))


--instance Functor (MState s) where

{-instance Monad (MState s) where
    (MState h) >>= f = MState $ \s -> let (a, newState) = h s
                                          (MState g) = f a
                                      in g newState
-}

instance Functor (GameState s) where
    --fmap f (GameState g st) = GameState g newState
        --where newState = f st
    fmap = liftM

instance Applicative (GameState s) where
    pure = return
    (<*>) = ap


instance Monad (GameState s) where
    return x = GameState $ \s -> (x,s)
    (GameState st) >>= f = GameState $ \s -> let (b, s2) = st s
                                                 (GameState g) = f b
                                            in   g s2
    --return x= GameState id
    --(GameState h) >>= f = GameState $ \s -> let (a,s2) =  h s
    --                                            (GameState g) = f a
    --                                        in g s2


makeGrid :: (Eq a, Num a) => [[a]] -> [[Maybe a]]
makeGrid g= fmap (\x -> if x==0 then Nothing else Just x) <$> g

instance Functor Grid where
    fmap f (Grid g) = Grid $ fmap f <$> g

instance Show (Grid Tile) where
    show = printBoard


printBoard :: Grid Tile -> String
printBoard (Grid g) = concatMap ((++"\n") . printRow) g
    where
        largestNum = length . show . maximum . catMaybes $ concat g
        printRow = unwords . map (padString . maybe "_" show)
        padding = flip subtract largestNum . length
        padString = (++) <$> id <*> (concat . flip replicate " " . padding)

sizeOfGrid :: Grid a -> Int
sizeOfGrid = (2*).length . getGrid

replace :: [a] -> Int -> a -> [a]
replace lst ind newData = before ++ [newData] ++ after
    where (before,_:after) = splitAt ind lst

updateGrid :: Grid Tile -> Pos -> Tile -> Grid Tile
updateGrid (Grid grid) (x, y) newData = Grid $ replace grid y newRow
    where oldRow = grid !! y
          newRow = replace oldRow x newData



getEmptyTiles :: Grid Tile -> [Pos]
getEmptyTiles (Grid grid) = fst <$> filteredGrid
    where
        numRows = length grid
        indexedGrid = zip [(x,y) | y <- [0..numRows-1], x <- [0..numRows-1]] $ concat grid
        filteredGrid = filter (isNothing . snd) indexedGrid


createBoard :: Int -> Grid Tile
createBoard =  (Grid .) . replicate <*> (`replicate` Nothing)



shiftBoard :: Move -> Grid Tile -> Grid Tile
shiftBoard R = shiftBoardRight
shiftBoard m = rotateRight . shiftBoardRight . rotateLeft
    where
        rotations = getRotation m
        rotate dir g = iterate (rotateBoard dir) g !! rotations
        rotateLeft = rotate RLeft
        rotateRight = rotate RRight

rotateBoard :: RotationDirection -> Grid a -> Grid a
rotateBoard RRight = Grid . transpose . reverse . getGrid
rotateBoard RLeft = Grid . reverse . transpose . getGrid


shiftBoardRight :: Grid Tile -> Grid Tile
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


addNums' :: Grid Tile -> Int -> Grid Tile
addNums' grid r = updateGrid grid newTile (pure 2)
    where emptyTiles = getEmptyTiles grid
          newTileInd = r `mod` length emptyTiles
          newTile = emptyTiles !! newTileInd



--addNums :: int -> GameState (GState (Grid Tile)) (Grid Tile)
addNums :: Int -> GState (Grid Tile) -> GState (Grid Tile)
addNums 0 gs = gs
addNums nums (GState grid gen) = addNums (nums-1) $ 
  let (r, newGen) = random gen
      newBoard = addNums' grid r
                    in GState newBoard newGen

liftGridFunc :: (a -> a) -> StateIO (GState a) IO (GState a)
liftGridFunc f = do
    gs <- get
    return $ f <$> gs
    


--bind :: (Grid Tile -> Grid Tile) -> ()


main :: IO ()
main = do
    y <- x newBoard
    return ()
        where newBoard = GState (createBoard 3) $ mkStdGen 100
              play2 = play $ addNums 2 newBoard
              x = runStateT play2

checkGameOver :: GState (Grid Tile) -> StateIO (GState (Grid Tile)) IO ()
checkGameOver gs@(GState grid _)
    | length (getEmptyTiles grid) == 1 = lift $ putStrLn "Game Over"
    | otherwise = play gs

play :: GState (Grid Tile) -> StateIO (GState (Grid Tile)) IO ()
play gs = do
    lift $ putStrLn $ getVal $ printBoard <$> gs
    put $ addNums 1 gs
    lift $ putStrLn "------------" 
    (GState a b) <- get
    lift $ putStr $ printBoard a
    move <- lift getMove
    case move of
        Nothing -> lift $ putStrLn "Game Over"
        Just m ->  liftGridFunc  (shiftBoard m) >>= checkGameOver

getMove :: IO (Maybe Move)
getMove = do
    putStrLn "Enter W, D, S, A, or Q to quit."
    inp <- getChar
    putStrLn ""
    maybe quit checkValid $ parseInput inp
    where
        quit = return Nothing
        checkValid = maybe getMove (return . Just)

parseInput :: Char -> Maybe (Maybe Move)
parseInput 'q' = Nothing
parseInput inp = Just $ parseMove inp

parseMove :: Char -> Maybe Move
parseMove 'w' = Just U
parseMove 'a' = Just L
parseMove 's' = Just D
parseMove 'd' = Just R
parseMove _ = Nothing