{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

import Data.Foldable (Foldable(foldl'))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Index (imapM)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Trans.State.Strict (State, execState, put, get)
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Control.Monad (replicateM_)

type Point = V3 Int

main :: IO ()
main = do
  input <- readFile "17.txt" <&> lines <&> parseInput
  -- print input
  print $ part1 input

  putStrLn "\n__Part 2"
  input2 <- readFile "17.txt" <&> lines <&> parseInput2
  print $ part2 input2

parseInput :: [[Char]] -> Set Point
parseInput xss = execState state S.empty
  where
    state = xss
      & imapM (\i xs -> xs
        & imapM (\j x -> do
          case x of
            '#' -> do
              world <- get
              put $ S.insert (V3 i j 0) world
            '.' -> return ()
            _ -> error "invalid input"
        )
      )

neighbours :: Point -> Set Point
neighbours x = S.map (x +) deltas
  where
    deltas = V3 <$> [0,1,-1] <*> [0,1,-1] <*> [0,1,-1]
      & S.fromList & S.delete (V3 0 0 0)

nextState :: State (Set Point) ()
nextState = next
  where
    next = do
      s0 <- get
      let
        f (s,ns) x = if activeNeibCount `elem` [2,3]
          then (S.insert x s, ns') else (s, ns')
          where
            n' = neighbours x
            activeNeibCount = S.size (S.intersection n' s0)
            ns' = ns <> n'
      -- first S.empty collects points to be turned on
      -- second S.empty collects neighbours
      let
        -- handle active cubes first
        (s1,ns1) = s0 & foldl' f (S.empty, S.empty)
        -- handle inacitve cubes
        inactives = ns1 S.\\ s0
        g s x = if activeNeibCount == 3 then S.insert x s else s
          where
            n' = neighbours x
            activeNeibCount = S.size (S.intersection n' s0)
      put $ foldl' g s1 inactives

solve1 :: Set Point -> Set Point
solve1 = execState (replicateM_ 6 nextState)

part1 :: Set Point -> Int
part1 = S.size . solve1


-- Part 2: copy-paste Part 1 and modify slightly

type Point4 = V4 Int

parseInput2 :: [[Char]] -> Set Point4
parseInput2 xss = execState state S.empty
  where
    state = xss
      & imapM (\i xs -> xs
        & imapM (\j x -> do
          case x of
            '#' -> do
              world <- get
              put $ S.insert (V4 i j 0 0) world
            '.' -> return ()
            _ -> error "invalid input"
        )
      )

neighbours2 :: Point4 -> Set Point4
neighbours2 x = S.map (x +) deltas
  where
    deltas = V4 <$> [0,1,-1] <*> [0,1,-1] <*> [0,1,-1] <*> [0,1,-1]
      & S.fromList & S.delete (V4 0 0 0 0)

nextState2 :: State (Set Point4) ()
nextState2 = next
  where
    next = do
      s0 <- get
      let
        f (s,ns) x = if activeNeibCount `elem` [2,3]
          then (S.insert x s, ns') else (s, ns')
          where
            n' = neighbours2 x
            activeNeibCount = S.size (S.intersection n' s0)
            ns' = ns <> n'
      -- first S.empty collects points to be turned on
      -- second S.empty collects neighbours
      let
        -- handle active cubes first
        (s1,ns1) = s0 & foldl' f (S.empty, S.empty)
        -- handle inacitve cubes
        inactives = ns1 S.\\ s0
        g s x = if activeNeibCount == 3 then S.insert x s else s
          where
            n' = neighbours2 x
            activeNeibCount = S.size (S.intersection n' s0)
      put $ foldl' g s1 inactives

solve2 :: Set Point4 -> Set Point4
solve2 = execState (replicateM_ 6 nextState2)

part2 :: Set Point4 -> Int
part2 = S.size . solve2
