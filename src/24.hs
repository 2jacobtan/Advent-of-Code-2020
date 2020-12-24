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

import Data.Foldable (Foldable(foldl'))
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Control.Arrow ((>>>))

import Linear.V2

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Maybe (fromMaybe)
import Data.Functor.Identity (Identity(..))
import qualified Data.Set as S

type Parser = Parsec Void String

main = do
  inputRaw <- readFile "24.txt"
  -- parseTest input1P inputRaw
  let input1 = fromMaybe (error "failParse") $ parseMaybe input1P inputRaw
  putStrLn "\n__Part 1"
  -- print $ solve1 input1
  print $ part1 input1


-- Hexagonal tiling can be represented as a 2D plane.

-- Parsing

data Dir = E | W | NE | NW | SE | SW

dirP :: Parser Dir
dirP =
  try "nw" $> NW <|>
  try "ne" $> NE <|>
  try "sw" $> SW <|>
  try "se" $> SE <|>
  try "w" $> W <|>
  try "e" $> E 
  
dirToVec :: Dir -> V2 Integer
dirToVec = \case
  E -> V2 1 0
  W -> V2 (-1) 0
  NW -> V2 0 1
  SE -> V2 0 (-1)
  NE -> V2 1 1
  SW -> V2 (-1) (-1)

dirVecP :: Parser (V2 Integer)
dirVecP = dirP <&> dirToVec

input1P :: Parser [[V2 Integer]]
input1P = sepBy1 (some dirVecP) eol 


-- Part 1

handleTile :: Ord a => S.Set a -> a -> S.Set a
handleTile tileSet tile = runIdentity $ S.alterF (Identity . not) tile tileSet

solve1 :: (Foldable t, Num a, Ord a) => [t a] -> S.Set a
solve1 tiles = map sum tiles & foldl' handleTile S.empty

part1 :: (Foldable t, Num a, Ord a) => [t a] -> Int
part1 tiles = S.size $ solve1 tiles


-- Part 2

