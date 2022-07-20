{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Data.List (transpose, intersperse)
import Data.Function(on)
import Data.Maybe
import System.Random
import StateIO
import Board
    ( getEmptyTiles, createBoard, shiftBoard, Grid, Move(..) )
import qualified Board as B(addNums)


import Control.Applicative (Applicative(liftA2), Alternative ((<|>)))
import Control.Monad.State (State, StateT (StateT), state, MonadTrans, MonadState)
import Data.Bits (Bits(shift))
import Control.Monad (ap, liftM, when)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.RWS (MonadState(get, put))


main :: IO ()
main = do
    runStateT ( addNums 2 >> play) newBoard
    return ()
        where newBoard = (createBoard 3, mkStdGen 100)

checkGameOver :: StateIO Grid IO ()
checkGameOver = StateIO $ \gs@(grid, _) -> do
    when (isGameOver grid) (putStrLn "Game Over")
    runStateT play gs
    where isGameOver = (==1) . length . getEmptyTiles
    
printBoard :: StateIO Grid IO ()
printBoard = StateIO $ \s -> do
    print $ fst s
    return ((), s)

shiftBoard' :: Monad m => Move -> StateIO Grid  m ()
shiftBoard' m = StateIO $ \(grid, gen) -> return ((), (shiftBoard m grid, gen))

play :: StateIO Grid IO ()--- StateIO (GState (Grid Tile)) IO ()
play = do
    printBoard
    addNums 1
    lift $ putStrLn "------------" 
    printBoard
    move <- lift getMove
    case move of
        Nothing -> lift $ putStrLn "Game Over"
        Just m -> shiftBoard' m >> checkGameOver

-- lift f = StateIO $ \gs -> return ((), f gs)

addNums :: Monad m => Int -> StateIO Grid m ()
addNums nums = StateIO $ \gs -> let (newBoard, newGen) = B.addNums nums gs
                                          in return ((), (newBoard, newGen))

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