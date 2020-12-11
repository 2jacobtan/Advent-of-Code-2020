-- Tried using Map. 3 times slower than 11.hs Array version.

-- {-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module Main where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (delete)
import Criterion.Main (whnf, bench, defaultMain)
import Data.Maybe (mapMaybe)
import Linear.V2 (V2(V2))
import Data.Map (elems, mapWithKey, fromAscList, Map, (!))

data Spot = O | E | F | B -- Occupied | Empty | Floor | Border (imaginary)
  deriving (Show, Eq)

type Grid = Map (V2 Int) Spot

main0 :: IO ()
main0 = do
  input0 <- readFile "11.txt" <&> lines
  let n = length . head $ input0
      m = length input0
      -- add borders
      input1 = replicate (n+2) 'B'
        : map ((++ ['B']) . ('B':)) input0
        ++ [replicate (n+2) 'B']
      parseChar = \case '#' -> O; 'L' -> E; '.' -> F; 'B' -> B
                        _ -> error "unrecognised char"
      input2 = input1 & concatMap (map parseChar)
      input = fromAscList $ zip (V2 <$> [0..(m+1)] <*> [0..(n+1)]) input2
  -- print <$> take 7 .  chunksOf (n+2) $ elems input
  -- print $ last .  chunksOf (n+2) $ elems input
  putStrLn "\n__Part 1"
  -- putStrLn "skipped to save time for part 2"
  print $ part1 (m,n) input

  putStrLn "\n__Part 2"
  print $ part2 (m,n) input

adjacents1 (V2 i j) = delete (V2 i j) [V2 i' j' | i' <- [i-1 .. i+1], j' <- [j-1 .. j+1]]
-- >>> adjacents1 (2,2)
-- [(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)]

solve1 :: (Int, Int) -> Grid -> Grid
solve1 (m,n) = go
  where
    go :: Grid -> Grid
    go grid =
      let nextGrid = mapWithKey nextSeatState grid 
       in if nextGrid == grid then grid else go nextGrid
      where
      shouldEmptySwap coord =
        adjacents1 coord & map (grid !) & notElem O
      shouldOccupiedSwap coord =
        adjacents1 coord & map (grid !) & length . filter (==O) & (>=4)
      nextSeatState coord seat = case seat of
        O -> if shouldOccupiedSwap coord then E else O
        E -> if shouldEmptySwap coord then O else E
        F -> F
        B -> B
-- >>> take 5 $ (,) <$> [0..2] <*> [0..2]
-- [(0,0),(0,1),(0,2),(1,0),(1,1)]

part1 :: (Int, Int) -> Grid -> Int
part1 gridSize grid0 = length . filter (==O) . elems $ solve1 gridSize grid0


-- Part 2

solve2 :: (Int, Int) -> Grid -> Grid
solve2 (m,n) grid0 = go grid0
  where
    range = (V2 0 0,V2 (m+1) (n+1))
    indices = V2 <$> [0..(m+1)] <*> [0..(n+1)]
    go grid =
      let nextGrid = mapWithKey nextSeatState grid
       in if nextGrid == grid then grid else go nextGrid
      where
      shouldEmptySwap coord =
        adjacents2 coord & notElem O
      shouldOccupiedSwap coord =
        adjacents2 coord & length . filter (==O) & (>=5)
      nextSeatState coord seat = case seat of
        O -> if shouldOccupiedSwap coord then E else O
        E -> if shouldEmptySwap coord then O else E
        F -> F
        B -> B
      adjacents2 :: V2 Int -> [Spot]
      adjacents2 (V2 i j) = linesOfSight ! V2 i j & map (grid !)

    linesOfSight = fromAscList $
      map los indices
      where
        los ij = (ij, mapMaybe (findLineOfSight ij) deltas)
        deltas = delete (0,0) $ (,) <$> [0,1,-1] <*> [0,1,-1]
        findLineOfSight ij _delta@(di,dj) =
          iterate (\(V2 x y) -> V2 (x+di) (y+dj)) ij
          & tail & findSeat
        findSeat =
          foldr (\ij r -> case grid0 ! ij of E -> Just ij; B -> Nothing; _ -> r)
            undefined
        -- Looking for a seat in each direction stops when we reach
        -- the imaginary border B.

part2 :: (Int, Int) -> Grid -> Int
part2 gridSize grid0 = length . filter (==O) . elems $ solve2 gridSize grid0

-- >>> delete (0,0) $ (,) <$> [0,1,-1] <*> [0,1,-1]
-- [(0,1),(0,-1),(1,0),(1,1),(1,-1),(-1,0),(-1,1),(-1,-1)]

main :: IO ()
main = do
  input0 <- readFile "11.txt" <&> lines
  let n = length . head $ input0
      m = length input0
      -- add borders
      input1 = replicate (n+2) 'B'
        : map ((++ ['B']) . ('B':)) input0
        ++ [replicate (n+2) 'B']
      parseChar = \case '#' -> O; 'L' -> E; '.' -> F; 'B' -> B
                        _ -> error "unrecognised char"
      input2 = input1 & concatMap (map parseChar)
      input = fromAscList $ zip (V2 <$> [0..(m+1)] <*> [0..(n+1)]) input2

  putStrLn "\n__Part 1"
  print $ part1 (m, n) input

  putStrLn "\n__Part 2"
  print $ part2 (m,n) input  

  defaultMain
    [
      bench "new" $ whnf (part2 (m,n)) input
    ]