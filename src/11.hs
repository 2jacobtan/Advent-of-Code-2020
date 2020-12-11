{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module Main where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Array ((!), Array, elems, listArray)
import Data.List (delete)

data Spot = O | E | F | B -- Occupied | Empty | Floor | Border (imaginary)
  deriving (Show, Eq)

type Grid = Array (Int, Int) Spot

main :: IO ()
main = do
  input0 <- readFile "11.txt" <&> lines
  let n = length . head $ input0
      m = length input0
      -- pad borders empty cells (=floor)
      input1 = replicate (n+2) 'B'
        : map ((++ ['B']) . ('B':)) input0
        ++ [replicate (n+2) 'B']
      parseChar = \case '#' -> O; 'L' -> E; '.' -> F; 'B' -> B
                        _ -> error "unrecognised char"
      input2 = input1 & concatMap (map parseChar)
      input = listArray ((0,0),(m+1,n+1)) input2
  -- print <$> take 7 .  chunksOf (n+2) $ elems input
  -- print $ last .  chunksOf (n+2) $ elems input
  putStrLn "\n__Part 1"
  putStrLn "skipped to save time for part 2"
  -- print $ part1 (m,n) input

  putStrLn "\n__Part 2"
  print $ part2 (m,n) input

adjacents1 (i,j) = delete (i,j) [(i',j') | i' <- [i-1 .. i+1], j' <- [j-1 .. j+1]]
-- >>> adjacents1 (2,2)
-- [(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)]

solve1 :: (Int, Int) -> Grid -> Grid
solve1 (m,n) = go
  where
    go grid =
      let nextGrid = listArray ((0,0),(m+1,n+1)) $
            (,) <$> [0..(m+1)] <*> [0..(n+1)]
            & map nextSeatState
       in if nextGrid == grid then grid else go nextGrid
      where
      shouldEmptySwap coord =
        adjacents1 coord & map (grid !) & notElem O
      shouldOccupiedSwap coord =
        adjacents1 coord & map (grid !) & length . filter (==O) & (>=4)
      nextSeatState coord = case grid ! coord of
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
solve2 (m,n) = go
  where
    go grid =
      let nextGrid = listArray ((0,0),(m+1,n+1)) $
            (,) <$> [0..(m+1)] <*> [0..(n+1)]
            & map nextSeatState
       in if nextGrid == grid then grid else go nextGrid
      where
      shouldEmptySwap coord =
        adjacents2 coord & notElem O
      shouldOccupiedSwap coord =
        adjacents2 coord & length . filter (==O) & (>=5)
      nextSeatState coord = case grid ! coord of
        O -> if shouldOccupiedSwap coord then E else O
        E -> if shouldEmptySwap coord then O else E
        F -> F
        B -> B
      adjacents2 :: (Int, Int) -> [Spot]
      adjacents2 (i,j) = directions
        where
          --- | first-draft; it worked but so ugly LOL
          -- north = [(i',j) | i' <- reverse [1..(i-1)] ]
          -- south = [(i',j) | i' <- [(i+1)..m] ]
          -- west = [(i,j') | j' <- reverse [1..(j-1)] ]
          -- east = [(i,j') | j' <- [(j+1)..n] ]
          -- northE = [(i',j') | i' <- reverse [1..(i-1)] | j' <- [(j+1)..n] ]
          -- southE = [(i',j') | i' <- [(i+1)..m] | j' <- [(j+1)..n] ]
          -- southW = [(i',j') | i' <- [(i+1)..m] | j' <- reverse [1..(j-1)] ]
          -- northW = [(i',j') | i' <- reverse [1..(i-1)] | j' <- reverse [1..(j-1)] ]
          -- directions =
          --   [north, south, west, east, northE, southE, southW, northW]
          --   <&> foldr (\ij r -> case grid ! ij of F -> r; x -> x) F
          directions =
            let deltas = delete (0,0) $ (,) <$> [0,1,-1] <*> [0,1,-1]
                findLineOfSight _delta@(di,dj) = tail $ iterate (\(x,y) -> (x+di,y+dj)) (i,j)
                linesOfSight = map findLineOfSight deltas
             in linesOfSight
                <&> foldr (\ij r -> case grid ! ij of F -> r; x -> x) F
                -- Looking for a seat in each direction stops when we reach
                -- the imaginary border B.

part2 :: (Int, Int) -> Grid -> Int
part2 gridSize grid0 = length . filter (==O) . elems $ solve2 gridSize grid0

-- >>> delete (0,0) $ (,) <$> [0,1,-1] <*> [0,1,-1]
-- [(0,1),(0,-1),(1,0),(1,1),(1,-1),(-1,0),(-1,1),(-1,-1)]
