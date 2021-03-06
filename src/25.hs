{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
import Data.List (iterate', find)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Math.NumberTheory.Moduli (powMod, Mod)

main = do
  putStrLn "Hello."
  print $ part1 ()

-- sample input
-- num1 = 5764801
-- num2 = 17807724

-- input
num1 = 9232416
num2 = 14144084

num12 = mod (num1 * num2) m

m = 20201227


iterMod b = iterate' (flip mod m . (*b)) b

--- Original version had a limit; found a solution with subject number 13 lol
-- findPow b num limit = find ((== num) . fst) (iterMod b `zip` [1..limit]) <&> snd

--- Fixed version with no limit
findPow b num limit = find ((== num) . fst) (iterMod b `zip` [1..]) <&> snd

-- >>> findPow 7 num12 m
-- Just 19

-- >>> findPow 7 num1 19
-- Just 8

-- >>> (7^11) `mod` m
-- 17807724

-- >>> findPow 7 num1 0
-- Just 8927518

-- >>> findPow 7 num2 0
-- Just 13240670

-- >>> powMod (7 :: Mod 20201227) (8927518*13240670)
-- (1478097 `modulo` 20201227)

--- ^ Part 1 answer is 1478097

-- *** everything below is unnecessary for Part 1 ***

findXY b limit = do
  x <- findPow b num1 limit
  if (b^(limit - x)) `mod` m == num2
    then Just (b, x, limit - x)
    else Nothing

solve1 b = do
  trace (show b) (return ())
  sumXY <- findPow b num12 m
  findXY b sumXY 

part1 _ = mapMaybe solve1 [7..100] & take 1 & head
  -- <&> (<&>
  -- (\sol1@(b, fromIntegral -> x, fromIntegral -> y) ->
  --   (
  --     sol1
  --     , iterMod b !! (x*y)
  --   ))
  -- )
