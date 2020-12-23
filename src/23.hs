{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module Main where

import Data.Foldable (foldrM, Foldable(foldl'))
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Data.Char (digitToInt)
import Control.Monad.Trans.State.Strict (execState, State, get, put)
import Data.Ord (comparing, Down(..))
import Data.List (find, sortBy)
import Control.Monad (when, replicateM_)
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
  -- print $ part2 inputSample 9 100
  -- print $ part2 inputActual 9 100
  print $ part2 inputActual (10^6) (10^7)
  

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

initSequence input = zip input (tail input ++ [head input])

-- >>> initSequence inputSample
-- >>> initSequence inputActual
-- [(3,8),(8,9),(9,1),(1,2),(2,5),(5,4),(4,6),(6,7),(7,3)]

-- [(6,1),(1,4),(4,7),(7,5),(5,2),(2,8),(8,3),(3,9),(9,6)]

-- Does NOT yet correctly connect base to extended range
fullSequence :: Integral a => [Int] -> Int -> [a]
fullSequence input fullLen = baseArray ++ [(length input + 2) .. (fullLen + 1)] & map fromIntegral
  where
    baseArray = initSequence input & sortBy (comparing fst) & map snd
-- >>> fullSequence inputActual 9
-- >>> fullSequence inputActual 12
-- [4,8,9,7,2,1,5,3,6]

-- [4,8,9,7,2,1,5,3,6,11,12,13]

-- >>> fullSequence inputSample 9
-- >>> fullSequence inputSample 12
-- [2,5,8,6,4,7,3,9,1]

-- [2,5,8,6,4,7,3,9,1,11,12,13]

startState :: [Int] -> Int -> ST s (STRef s Int32, STUArray s Int32 Int32)
startState input fullLen = do
  startCup <- newSTRef $ fromIntegral $ head input
  startCircle <- newListArray (1, fromIntegral fullLen)
    (fullSequence input fullLen)

  when (fullLen > length input) $ do
    -- connect base to extended
    writeArray startCircle (last input & fromIntegral) (length input + 1 & fromIntegral)
    writeArray startCircle (fullLen & fromIntegral) (head input & fromIntegral)

  return (startCup, startCircle)

nextState :: forall s . STRef s Int32 -> STUArray s Int32 Int32 -> ST s ()
nextState currentRef circleRef = do
  current <- readSTRef currentRef
  -- trace ("current: " ++ show current) (return ())

  -- pick up
  c1 <- readCircle current
  c2 <- readCircle c1
  c3 <- readCircle c2
  let pickedUp = [c1,c2,c3]
  -- trace ("pickedUp: " ++ show pickedUp) (return ())
  c4 <- readCircle c3
  writeCircle current c4 
    -- link up current to c4, hence "closing" the circle after pickup
  
  (b,b') <- getBounds circleRef
  let destination = fromMaybe (error "impossibru") $
        find (`notElem` pickedUp) $ [(current-1), (current-2) .. 1]
          ++ [b', (b'-1) .. (-1)]  -- (-1) for sanity
  -- trace ("destination: " ++ show destination) (return ())


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
  -- trace ("startState: " ++ show current0 ++ show circle0) (return ())
  
  replicateM_ rounds (nextState currentRef circleRef)
  current <- readSTRef currentRef
  
  let
    readCircle :: Int32 -> ST s Int32
    readCircle = readArray circleRef

  -- iterateLimit 8 readCircle 1  -- for Part 1
  
  required <- drop 1 <$> iterateLimit 2 readCircle 1  -- for Part 2

  trace ("c1,c2: " ++ show required) (return ())
  return required

part2 :: [Int] -> Int -> Int -> Integer
part2 input fullLen rounds = (product . map fromIntegral) $
  runST $ solve2 input fullLen rounds

-- copied from https://hackage.haskell.org/package/utility-ht-0.0.15/docs/src/Control.Monad.HT.html#iterateLimit
iterateLimit :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateLimit m f =
   let aux n x =
          fmap (x:) $
          if n==0
            then return []
            else aux (n-1) =<< f x
   in  aux m
