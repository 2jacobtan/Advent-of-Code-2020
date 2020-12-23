{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module Main where

import Data.Foldable (Foldable(foldl'))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Data.Char (digitToInt)
import Control.Monad.Trans.State.Strict (execState, State, get, put)
import Data.Ord (comparing, Down(..))
import Data.List (find, sortBy)
import Control.Monad (replicateM_)
import Control.Monad.ST (runST,  ST )
import Data.Array.MArray (getElems, MArray(getBounds), writeArray,  newListArray, readArray )
import Data.Array.ST (STUArray)
import Debug.Trace (trace)
import Data.Int (Int32)
import Data.STRef (writeSTRef, readSTRef, newSTRef, STRef)
import Data.Maybe (fromMaybe)

inputSample = "389125467" & map digitToInt
inputActual = "614752839" & map digitToInt

main :: IO ()
main = do
  -- print $ solve1 inputSample
  -- print $ part1 inputSample

  print $ part1 inputActual

  putStrLn "\n__Part 2"
  print $ part2 inputActual 9 1
  

move :: State [Int] ()
move = do
  (splitAt 1 ->
    (head -> current,
    splitAt 3 -> (pickup, rest))) <- get
  let cRest = current : rest
  let candidates = sortBy (comparing (Down . fst)) (zip cRest [1..]) -- deliberate offset by 1
  let targetIx = case find ((< current) . fst) candidates of
        Just (x,i) -> i  -- & trace (show x ++ ", " ++ show i)
        Nothing -> snd $ head candidates
  let (before, after) = splitAt targetIx cRest -- offset by 1 above works here for splitAt
  let afterPlacement = before ++ pickup ++ after
  -- trace (show before ++ show pickup ++ show after) (pure ())
  let newArrangement = tail afterPlacement ++ [head afterPlacement]
  put newArrangement

solve1 :: [Int] -> [Int]
solve1 = execState (replicateM_ 100 move)

part1 :: [Int] -> [Char]
part1 input = solve1 input & cycle
  & dropWhile (/= 1) & take (length input) & tail
  & concatMap show


-- Part 2

-- Gotta implement a graph (cyclic linked list) after all.

circleLength = 10^6

initSequence input = zip input (tail input) & sortBy (comparing fst)
  -- & (++ [(last input, length input + 1)])
initArray input = initSequence input & map snd
-- >>> initSequence inputActual
-- >>> initArray inputActual
-- [(1,4),(2,8),(3,9),(4,7),(5,2),(6,1),(7,5),(8,3)]

-- [4,8,9,7,2,1,5,3]

fullSequence :: Integral a => [Int] -> Int -> [a]
fullSequence input fullLen = initArray input ++ [(length input + 1) .. fullLen] ++ [head input] & map fromIntegral
-- >>> fullSequence inputActual 9
-- >>> fullSequence inputActual 12
-- [4,8,9,7,2,1,5,3,6]

-- [4,8,9,7,2,1,5,3,10,11,12,6]

startState :: [Int] -> Int -> ST s ((STRef s Int32), (STUArray s Int32 Int32))
startState input fullLen = do
  startCup <- newSTRef $ fromIntegral $ head input
  startCircle <- newListArray (1, fromIntegral fullLen)
    (fullSequence input (fromIntegral fullLen))
  return (startCup, startCircle)

nextState :: forall s . STRef s Int32 -> STUArray s Int32 Int32 -> ST s ()
nextState currentRef circleRef = do
  current <- readSTRef currentRef

  -- pick up
  c1 <- readCircle current
  c2 <- readCircle c1
  c3 <- readCircle c2
  let pickedUp = [c1,c2,c3]
  c4 <- readCircle c3
  writeCircle current c4 
    -- link up current to c4, hence "closing" the circle after pickup
  
  (b,b') <- getBounds circleRef
  let destination = fromMaybe (error "impossibru") $ find (`notElem` pickedUp) $ [current, (current-1) .. 1]
        ++ [b', (b'-1) .. (-1)]  -- (-1) for sanity

  -- insert pickedUp into clockwise of destination
  destination1 <- readCircle destination
  writeCircle destination c1
  writeCircle c3 destination1

  -- select new current cup
  newCurrentCup <- readCircle current
  writeSTRef currentRef newCurrentCup

  where
    readCircle :: Int32 -> ST s Int32
    readCircle = readArray circleRef
    writeCircle :: Int32 -> Int32 -> ST s ()
    writeCircle = writeArray circleRef

solve2 :: forall s . [Int] -> Int -> Int -> ST s [Int32]
solve2 input fullLen rounds = do
  (currentRef, circleRef) <- startState input fullLen
  current0 <- readSTRef currentRef
  circle0 <- getElems circleRef
  trace ("startState: " ++ show circle0) (return ())
  
  replicateM_ rounds (nextState currentRef circleRef)
  current <- readSTRef currentRef
  
  let
    readCircle :: Int32 -> ST s Int32
    readCircle = readArray circleRef

  c <- readCircle current
  c1 <- readCircle c
  c2 <- readCircle c1
  c3 <- readCircle c2
  c4 <- readCircle c3
  c5 <- readCircle c4
  c6 <- readCircle c5
  c7 <- readCircle c6
  c8 <- readCircle c7
  c9 <- readCircle c8

  return [c1,c2,c3,c4,c5,c6,c7,c8,c9]

part2 :: [Int] -> Int -> Int -> [Int32]
part2 input fullLen rounds = runST $ solve2 input fullLen rounds
