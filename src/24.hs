{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

import Data.Foldable (Foldable(foldl'))
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Control.Arrow ((>>>))

import Linear.V2 ( V2(..) )

import Text.Megaparsec
    ( (<|>), sepBy1, some, MonadParsec(try), parseMaybe, Parsec )
import Text.Megaparsec.Char ( eol )
import Data.Void (Void)
import Data.Maybe (fromMaybe)
import Data.Functor.Identity (Identity(..))
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Parser = Parsec Void String

main :: IO ()
main = do
  inputRaw <- readFile "24.txt"
  -- parseTest input1P inputRaw
  let input1 = fromMaybe (error "failParse") $ parseMaybe input1P inputRaw
  putStrLn "\n__Part 1"
  -- print $ solve1 input1
  print $ part1 input1

  putStrLn "\n__Part 2"
  
  inputRawSample <- readFile "24sample.txt"
  let inputSample = fromMaybe (error "failParse") $ parseMaybe input1P inputRawSample
  let sampleSolution = solve2sample inputSample
  print $ sampleSolution & take 11
  print $ map (sampleSolution !!) [20,30 .. 100]
  
  print $ part2 input1


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

adjacents :: V2 Integer -> [V2 Integer]
adjacents tile = tile : map (+ tile) allDirs -- include itself
  where
    allDirs = [E , W , NE , NW , SE , SW] & map dirToVec

blackNeigboursMap :: (Foldable t, Num a) => t (V2 Integer) -> M.Map (V2 Integer) a
blackNeigboursMap tileSet = concatMap (map (,1) . adjacents) tileSet
  & M.fromListWith (+)

eachDay :: S.Set (V2 Integer) -> S.Set (V2 Integer)
eachDay tileSet = keepBlack <> turnBlack
  where
    blackNeibsMap = blackNeigboursMap tileSet
    blackTilesMap = blackNeibsMap `M.restrictKeys` tileSet
    whiteTilesMap = blackNeibsMap `M.withoutKeys` tileSet
    keepBlack =
      M.filter (not . (\x -> x == (0 + 1) || x > 2 + 1)) blackTilesMap & M.keysSet
        -- +1 cuz black self is included
    turnBlack = M.filter (==2) whiteTilesMap
      & M.keysSet


solve2 :: S.Set (V2 Integer) -> S.Set (V2 Integer)
solve2 tiles = tiles & foldr (.) id (replicate 100 eachDay)

part2 :: Foldable t => [t (V2 Integer)] -> Int
part2 tiles = solve1 tiles & solve2 & S.size

solve2sample :: Foldable t => [t (V2 Integer)] -> [Int]
solve2sample tiles = solve1 tiles & iterate eachDay & map S.size

