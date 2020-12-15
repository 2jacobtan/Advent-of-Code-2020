{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module Main where

import Data.Function ((&))

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Debug.Trace as Debug
import Data.Int (Int32)
import Control.Monad.ST (runST, ST)
-- import Control.Monad (foldM)
import Criterion.Main (whnf, bench, defaultMain)

input = [8,0,17,4,1,12]

dict :: IntMap Int
dict = Map.fromList $ zip (init input) [0..]

solve1 :: Int -> Int
solve1 (subtract 2 -> n) = foldr f (\_ x -> x) [(length input - 1)..n] dict (last input)
  where
    f i r d x = r (Map.insert x i d) next   & (if mod i 1000000 == 0 then Debug.trace (show (i,x)) else id)
      where
          next = case Map.lookup x d of
                        Just j -> i - j
                        Nothing -> 0

part1 :: Int -> Int
part1 = solve1

main :: IO ()
main = do
  -- to correct off-by-one
  -- print $ map part1 [125..150]
  -- print $ map part2 [125..150]

  -- print $ map part1 [2010..2019]
  -- print $ map part2 [2010..2019]

  -- print $ part1 (3*10^7) -- a bit slow (2 minutes) because Map
  
  print $ part2 (3*10^7) -- fast (<2 seconds) because Vector (Array)

  defaultMain
    [
      bench "Day15 part2" $ whnf part2 (3*10^7)
    ]  

-- Part 2

solve2 :: Int -> ST s Int
solve2 (subtract 1 -> n) = do
  vec :: V.MVector s Int32 <- V.new n

  -- populate vec with starting numbers
  mapM_ (\(x,i) -> V.write vec x (fromIntegral i)) $ zip input [1..]
  
  let
    f :: Int -> Int32 -> ST s Int
    f x i = do
        j <- V.read vec x
        let next = fromIntegral $ case j of
              0 -> 0
              j -> i - j
        V.write vec x i
        return next  -- & (if mod i 1000000 == 0 then Debug.trace (show (i,x)) else id)
  
  -- foldM f (fromIntegral $ last input) [(fromIntegral $ length input)..(fromIntegral n)]
  --- ^ slowest

  let
    go num ind
      | ind > n32 = num
      | otherwise =
          num >>= \num' -> go (f num' ind) (ind + 1)
  
  go (return $ fromIntegral $ last input) (fromIntegral $ length input)
  --- ^ faster (see Day15.hs for fastest implementation with StateT)
  
  where
    n32 :: Int32 = fromIntegral n

part2 :: Int -> Int
part2 n = runST $ solve2 n
