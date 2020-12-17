{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

import Data.Foldable (Foldable(foldl'))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Data.List.Index (imapM, imap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Trans.State.Strict (State, execState, put, get)
import Linear.V3 (V3(..))
import Control.Monad (replicateM_)

type Point = V3 Int

main :: IO ()
main = do
  input <- readFile "17.txt" <&> lines <&> parseInput
  -- print input
  print $ part1 input

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

