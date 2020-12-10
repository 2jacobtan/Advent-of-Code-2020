{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort, foldl')
import qualified Debug.Trace as Debug
import Data.Monoid (Product(getProduct, Product))

main = do
  input <- readFile "10.txt" <&> lines <&> map (read @Int)
  print $ solve1 input
  print $ part1 input
  
  putStrLn "\n__Part 2"
  let cogs@Cogs{..} = findCogs input
  print cogs
  print $ sum cs + streak
  print $ solve2 input

-- | Joltage, count 1-jolt differences, count 3-jolt differences
data A = A {j :: !Int, c1 :: !Int, c3 :: !Int} deriving Show

solve1 :: [Int] -> A
solve1 adapters = foldl' f (A 0 0 0) $ sort adapters
  where
    f a@A{..} x = 
      case x - j of
        1 -> a {j = x, c1 = succ c1}
        3 -> a {j = x, c3 = succ c3}
        2 -> a {j = x} & Debug.trace "2-gap spotted"
        _ -> error ("joltage gap too large: " ++ show x ++ " " ++ show j)

part1 :: [Int] -> Int
part1 (solve1 -> A{..}) = c1 * (c3 + 1)

-- Part 2
-- theory:
{-
There are no 2-jolt gaps.
3-gaps cannot be changed.
Consecutive 1-gaps can be changed:
  0-1-2: can remove '1'.
    => 2 consec 1-gaps becomes 0 consec 1-gaps
  0-1-2-3: can remove '1' or '2'.
    => each time, 3 consec 1-gaps becomes either
    0 cog (= "consec 1-gaps") and 1 cog, or 1 cog and 0 cog.
General pattern:
  n cogs can be split into 0 cog and (n-2) cog, 1 cog and (n-3) cog, etc.
  This appears to be amenable to memoisation / dynamic programming.

Workable method:
  To calculate the possibilities available in n cogs,
  consider keeping the first droppable adapter, and the possibilities
  available in the resultant n-1 cogs.

  Also, consider dropping the first droppable adapter, and the
  possibilities available in the remaining n-2 cogs.

  Finally, consider dropping the first two droppable adpatres, and the
  possibilities available in the remaining n-3 cogs.
-}

data Cogs = Cogs {i :: !Int, cs :: [Int], streak :: !Int} deriving Show

findCogs adapters = foldl' f Cogs{i = 0, cs=[], streak=0} $ sort adapters
  where
    f cogs@Cogs{..} x = 
      case x - i of
        1 -> cogs{i = x, streak = succ streak} -- increment streak if 1-gap
        3 -> cogs{i = x, cs = streak : cs, streak = 0} -- reset streak if 3-gap
        2 -> cogs & Debug.trace "2-gap spotted"
        _ -> error ("joltage gap too large: " ++ show x ++ " " ++ show i)

cogPossibilities :: [Integer]
cogPossibilities = 1:1:2:genCogPoss 3

genCogPoss :: Int -> [Integer]
genCogPoss n = possN : genCogPoss (succ n)
  where
    possN = cogPossibilities !! (n-1)
      + cogPossibilities !! (n-2)
      + cogPossibilities !! (n-3)

solve2 :: [Int] -> Integer
solve2 = getProduct . foldMap (Product . (cogPossibilities !!))
  . ((:) <$> streak <*> cs) . findCogs

