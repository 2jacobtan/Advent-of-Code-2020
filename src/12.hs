{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module Day12 where

-- import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Linear.V2 (V2(V2), perp)
import Linear.Vector ((^*))
-- import qualified Debug.Trace as Debug

main :: IO ()
main = do
  input <- readFile "12.txt" <&> lines <&> map parseLine
  putStrLn "\n__Part 1"
  print $ part1 input
  
  putStrLn "\n__Part 2"
  print $ part2 input

  putStrLn ""

data Move = C (V2 Int) | T Int |  F Int -- Cardinal | Turn | Forward

parseLine :: [Char] -> Move
parseLine line@(move:(read @Int -> n)) =
  case move of
    'N' -> C $ V2 0 1 ^* n
    'S' -> C $ V2 0 (-1) ^* n
    'E' -> C $ V2 1 0 ^* n
    'W' -> C $ V2 (-1) 0 ^* n
    'L' -> T $ n `div` 90 `mod` 4
    'R' -> T $ (- n `div` 90) `mod` 4
    'F' -> F n
    _ -> error $ "invalid line: " ++ line
parseLine [] = error "empty line"

data Ship = S {dir :: V2 Int, man :: V2 Int, count :: !Int} -- direction, manhattan distance
  deriving Show

solve1 :: [Move] -> Ship
solve1 = foldl' f S{dir = V2 1 0, man = V2 0 0, count = 0}
  where
    f s@S{..} = \case
      C v -> s{man = man + v, count = count + 1} -- & \x -> Debug.trace (show x) x
      T n -> s{dir = perpN n dir, count = count + 1} -- & \x -> Debug.trace (show x) x
      F n -> s{man = man + dir ^* n, count = count + 1} -- & \x -> Debug.trace (show x) x

fpow :: (Eq t, Num t, Ord t) => (b -> b) -> t -> b -> b
fpow _f 0 = id
fpow _f n | n<0 = error "negative fpow"
fpow f n = f . fpow f (n-1)
perpN :: Int -> V2 Int -> V2 Int
perpN = fpow perp

part1 :: [Move] -> Int
part1 (solve1 -> S{man = V2 x y}) = abs x + abs y

-- Part 2

-- | For Part 2, reuse dir to represent ship displacement,
--   man to represent waypoint displacement relative to ship
solve2 :: [Move] -> Ship
solve2 = foldl' f S{dir = V2 0 0, man = V2 10 1, count = 0}
  where
    f s@S{..} = \case
      -- no change from part 1
      C v -> s{man = man + v, count = count + 1} -- & \x -> Debug.trace (show x) x
      -- rotate man instead
      T n -> s{man = perpN n man, count = count + 1} -- & \x -> Debug.trace (show x) x
      -- man and dir exchange places
      F n -> s{dir = dir + man ^* n, count = count + 1} -- & \x -> Debug.trace (show x) x

-- | Use dir instead of man.
part2 :: [Move] -> Int
part2 (solve2 -> S{dir = V2 x y}) = abs x + abs y
