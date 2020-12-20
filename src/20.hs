{-# LANGUAGE TupleSections #-}
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
import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import Data.Array
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = do
  inputRaw <- readFile "20.txt"
  -- parseTest inputP inputRaw
  let input = parseMaybe inputP inputRaw
  -- print input
  putStrLn "\n__Part 1"
  print $ length <$> input
  print $ part1 <$> input

type Parser = Parsec Void String
type IdArray = (Int, Array (Int,Int) Bool)

inputP :: Parser [IdArray]
inputP = sepBy1 tileP eol

tileP :: Parser IdArray
tileP = do
  id <- "Tile " *> decimal <* ":" <* eol
  lines <- endBy1 (some pixelP) eol
  return (id, listArray ((0,0),(9,9)) (concat lines))

pixelP :: Parser Bool
pixelP = printChar <&> (== '#')


-- Part 1

-- *** The edge on a tile matches with the reversed edge of another tile.

edges :: [[(Int, Int)]]
edges = [
  (0,) <$> [0..9],
  (9,) <$> [9,8..0],
  (,0) <$> [9,8..0],
  (,9) <$> [0..9]
  ]

-- reversed edges
edgesRev :: [[(Int, Int)]]
edgesRev = [
  (0,) <$> [9,8..0],
  (9,) <$> [0..9],
  (,0) <$> [0..9],
  (,9) <$> [9,8..0]
  ]

-- [Int] for array IDs, Int for counter
arrEdges :: IdArray -> [([Bool],([Int],Int))] 
arrEdges (id, arr) = (,([id],1)) . ((arr !) <$>) <$> edges

arrEdgesRev :: IdArray -> [([Bool],([Int],Int))] 
arrEdgesRev (id, arr) = (,([id],1)) . ((arr !) <$>) <$> edgesRev

mkEdgeMap :: [IdArray] -> M.Map [Bool] ([Int],Int)
mkEdgeMap arrs = M.fromListWith (\(x,n) (y,m) -> (x++y,n+m))
  $ concatMap arrEdges (arrs :: [IdArray])

-- with edges in reverse
mkEdgeMap' :: [IdArray] -> M.Map [Bool] ([Int],Int)
mkEdgeMap' arrs = M.fromListWith (\(x,n) (y,m) -> (x++y,n+m))
  $ concatMap arrEdgesRev (arrs :: [IdArray])

solve1 :: [IdArray] -> M.Map [Bool] ([Int],Int)
solve1 idArrays =
  M.unionWith (\(x,n) (y,m) -> (x++y,n+m)) (mkEdgeMap idArrays) (mkEdgeMap' idArrays)
  & M.filter ((==1) . snd)

part1 :: [IdArray] -> Int
part1 idArrays = solve1 idArrays & M.elems & map fst
  & M.fromListWith (+) . map (,1)
  & M.filter (==4)
  & M.keys & concat & product