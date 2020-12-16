{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module Day15 where

import Data.Function ((&))

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Tuple.Strict (T2(..), sfst, ssnd)
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Debug.Trace as Debug
import Data.Int (Int32)
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (runST, ST)
import Criterion.Main (whnf, bench, defaultMain)
import Control.Monad.Trans.State.Strict (put, get, gets, StateT(..), evalStateT)
import Data.Foldable (for_)
import Data.Functor ((<&>))

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

  -- print $ map part1 [2010..2020]
  -- print $ map part2 [2010..2020]

  -- print $ part1 (3*10^7) -- a bit slow (2 minutes) because Map
  
  print $ part2 (3*10^7) -- fast (<2 seconds) because Vector (Array) and StateT instead of foldM over a list

  defaultMain
    [
      bench "Day15 part2" $ whnf part2 (3*10^7)
    ]  

-- Part 2

-- | Added StateT method instead of my foldM method, copying Justin Le's code:
--   https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day15.hs

solve2 :: Int -> StateT (T2 Int Int32) (ST s) Int
solve2 n = do
  vec :: V.MVector s Int32 <- V.new n

  -- populate vec with starting numbers
  for_ input $ \y -> StateT $ \(T2 _ i) -> V.write vec y (i+1) <&> (, T2 y (i+1))
  
  whileM_ (gets ((< n32) . ssnd)) $ f vec
  gets sfst
  where
    n32 :: Int32 = fromIntegral n
{-# INLINE solve2 #-}

f :: V.MVector s Int32 -> StateT (T2 Int Int32) (ST s) ()
f vec = do
    (T2 x i) <- get
    j <- V.read vec x
    let next = fromIntegral $ case j of
          0 -> 0
          j -> i - j
    V.write vec x i
    put $ T2 next (i+1)  -- & (if mod i 1000000 == 0 then Debug.trace (show (i,x)) else id)
{-# INLINE f #-}

part2 :: Int -> Int
part2 n = runST $ flip evalStateT (T2 0 0 :: T2 Int Int32) $ solve2 n
