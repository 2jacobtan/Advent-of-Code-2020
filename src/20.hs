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

import Data.Foldable (asum, find, Foldable(foldl'))
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Control.Arrow ((>>>))
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import Data.Array
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Map (Map)

main :: IO ()
main = do
  inputRaw <- readFile "20.txt"
  -- parseTest inputP inputRaw
  let input = parseMaybe inputP inputRaw
  -- print input
  putStrLn "\n__Part 1"
  print $ length <$> input
  print $ part1 <$> input
  let inputMap = M.fromList <$> input
  print $ topLeft <$> inputMap

type Parser = Parsec Void String
type IdArray = (Int, Tile)
type Tile = Array (Int,Int) Bool
type ID = Int

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

part1 idArrays = solve1 idArrays & M.elems & map fst
  & M.fromListWith (+) . map (,1)
  & M.filter (==4)
  & M.keys & concat -- & product


-- Part 2

data Dir = N | S | W | E  -- north south west east
  deriving Show
type EdgeIx = [(Int,Int)]
type Edge = [Bool]
edgeN = (0,) <$> [0..9]  -- north
edgeS = (9,) <$> [9,8..0]  -- south
edgeW = (,0) <$> [9,8..0]  -- west
edgeE = (,9) <$> [0..9]  -- east

getEdge :: Tile -> EdgeIx -> Edge
getEdge arr edge = (arr !) <$> edge

-- corners (from Part 1): [1693,2111,2207,2339]
topLeft tiles = [1693,2111,2207,2339]
  & mapMaybe (\x -> (matchT tiles x edgeE *> matchT tiles x edgeS) <&> (,x))

tileToList :: Tile -> [[Bool]]
tileToList tile = elems tile & chunksOf 10

-- | align matched tile1 to tile0
orient E arr = \case
  W -> tileToList arr -- tile0 E matches tile1 W so already aligned
  E -> tileToList arr & reverse & map reverse -- 180
  N -> tileToList arr & transpose & reverse -- counter 90
  S -> tileToList arr & reverse & transpose -- 90
orient S arr = \case
  N -> tileToList arr -- tile0 S matches tile1 N so already aligned
  S -> tileToList arr & reverse & map reverse -- 180
  E -> tileToList arr & transpose & reverse -- counter 90
  W -> tileToList arr & reverse & transpose -- 90

orient S = \case

orient e = error "invalid dir0"

-- | Usage example: match tiles id ew, match tiles id sn
matchT :: Map ID Tile -> ID -> EdgeIx -> Maybe (Dir, IdArray)
matchT tiles id e0 = asum [
  findE edgeN <&> (N,),
  findE edgeS <&> (S,),
  findE edgeW <&> (W,),
  findE edgeE <&> (E,)
  ]
  where
    findE e1 = find (\(_id, x) ->
      getEdge (tiles M.! id) e0
      == reverse (getEdge x e1)) (M.assocs tiles)
