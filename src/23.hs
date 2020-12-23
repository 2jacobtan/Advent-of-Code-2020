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
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Debug.Trace (trace)
import Data.Int (Int32)

inputSample = "389125467" & map digitToInt
inputActual = "614752839" & map digitToInt

main :: IO ()
main = do
  -- print $ solve1 inputSample
  -- print $ part1 inputSample

  print $ part1 inputActual

  putStrLn "\n__Part 2"
  

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

-- Gotta implement a graph after all.

circleLength = 10^6

initSequence input = zip input (tail input) & sortBy (comparing fst)
  & (++ [(last input, length input + 1)])
initArray input = initSequence input & map snd
-- >>> initSequence inputActual
-- >>> initArray inputActual

fullSequence :: Integral a => [Int] -> Int -> [a]
fullSequence input fullLen = initArray input ++ [(length input + 2) .. fullLen] ++ [head input] & map fromIntegral
-- >>> fullSequence inputActual 9
-- >>> fullSequence inputActual 12

startState :: [Int] -> Int -> ST s ((STUArray s) Int32 Int32)
startState input fullLen = newListArray (1, fromIntegral fullLen) (fullSequence input (fromIntegral fullLen))

