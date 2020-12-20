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
import Data.List (foldl1', unfoldr, transpose)
import Control.Monad (replicateM_)
import Data.List.Split (chunksOf)
import Debug.Trace (trace)
import Data.Bits (Bits(xor))
import qualified Data.Matrix as Mat
import Prelude hiding (GHC.Show.Bool)

main :: IO ()
main = do
  inputRaw <- readFile "20.txt"
  -- parseTest inputP inputRaw
  let input = parseMaybe inputP inputRaw
  -- print input
  putStrLn "\n__Part 1"
  print $ length <$> input
  print $ part1 <$> input
  
  putStrLn "\n__Part 2"
  let inputMap = M.fromList $ fromMaybe (error "failParse") input
  -- print $ topLeft <$> inputMap -- == 2207
  putStrLn $ solve2alignment inputMap  -- check alignment

  seaMonster <- readFile "20b.txt" <&> lines <&> map (map (=='#'))
  seaMonsterString <- readFile "20b.txt" <&> lines
  mapM_ putStrLn seaMonsterString


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
topLeft :: Map ID Tile -> [(((Dir,Bool), IdArray), ID)]
topLeft tilesMap = [1693,2111,2207,2339]
  & mapMaybe (\x -> (matchT tilesMap x edgeE *> matchT tilesMap x edgeS) <&> (,x))

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
orient e _ = error "invalid dir0"

-- | Usage example: match tiles id edgeE, match tiles id edgeS
matchT :: Map ID Tile -> ID -> EdgeIx -> Maybe ((Dir,Bool), IdArray)
matchT tiles id0 e0 = asum [
  findE edgeN <&> ((N,True),),
  findE edgeS <&> ((S,True),),
  findE edgeW <&> ((W,True),),
  findE edgeE <&> ((E,True),),
  findE' edgeN <&> ((N,False),), -- False => matched tile needs to be flipped
  findE' edgeS <&> ((S,False),),
  findE' edgeW <&> ((W,False),),
  findE' edgeE <&> ((E,False),)
  ]
  where
    findE e1 = find (\(id1, x) -> id0 /= id1 &&
      getEdge (tiles M.! id0) e0
      == reverse (getEdge x e1)) (M.assocs tiles)
    -- findE' => matched tile needs to be flipped
    findE' e1 = find (\(id1, x) -> id0 /= id1 &&
      getEdge (tiles M.! id0) e0
      == getEdge x e1) (M.assocs tiles)

-- | Usage example: match tiles tile id edgeE, match tiles tile id edgeS
matchTbyArr :: Map ID Tile -> Tile -> ID -> EdgeIx -> Maybe ((Dir,Bool), IdArray)
matchTbyArr tiles tile0 id0 e0 = asum [
  findE edgeN <&> ((N,True),),
  findE edgeS <&> ((S,True),),
  findE edgeW <&> ((W,True),),
  findE edgeE <&> ((E,True),),
  findE' edgeN <&> ((N,False),), -- False => matched tile needs to be flipped
  findE' edgeS <&> ((S,False),),
  findE' edgeW <&> ((W,False),),
  findE' edgeE <&> ((E,False),)
  ]
  where
    findE e1 = find (\(id1, x) -> id0 /= id1 &&
      getEdge tile0 e0
      == reverse (getEdge x e1)) (M.assocs tiles)
    -- findE' => matched tile needs to be flipped
    findE' e1 = find (\(id1, x) -> id0 /= id1 &&
      getEdge tile0 e0
      == getEdge x e1) (M.assocs tiles)

makeTopRow :: Map ID Tile -> ID -> [([[Bool]], ID)] -- Bool to track if noFlip
makeTopRow tilesMap topLeftTileId = (tileToList topLeftTile, topLeftTileId) : rest
  where
    topLeftTile = tilesMap M.! topLeftTileId
    rest = unfoldr f (topLeftTile, 11, topLeftTileId)
    f :: (Tile, Integer, ID)
      -> Maybe (([[Bool]], Int), (Tile, Integer, Int))
    f (_,n,_) | n == 0 = Nothing
    f (tile0, n, id0) =
      Just ((oriented1, id1), (oriented1Arr, n-1, id1))
      -- & trace ("\n" ++ show dir1)
      where
        ((dir1,noFlip),(id1,arr1)) = fromMaybe (error "no match") $ matchTbyArr tilesMap tile0 id0 edgeE
        oriented1 = orient E arr1 dir1 & (if noFlip then id else reverse)
        oriented1Arr = listArray ((0,0),(9,9)) (concat oriented1)

makeColumn :: Map ID Tile -> ([[Bool]], ID) -> [([[Bool]], ID)]
makeColumn tilesMap (topTile, topTileId) =
  (topTile, topTileId) : unfoldr f (topTile', 11, topTileId)
  where
    topTile' = listArray ((0,0),(9,9)) $ concat topTile
    f :: (Tile, Integer, ID)
      -> Maybe (([[Bool]], Int), (Tile, Integer, Int))    
    f (_,n,_) | n == 0 = Nothing
    f (tile0, n, id0) =
      Just ((oriented1, id1), (oriented1Arr, n-1, id1))
      -- & trace ("\n" ++ show dir1)
      where
        ((dir1,noFlip),(id1,arr1)) = fromMaybe (error "no match") $ matchTbyArr tilesMap tile0 id0 edgeS
        oriented1 = orient S arr1 dir1 & (if noFlip then id else map reverse)
        oriented1Arr = listArray ((0,0),(9,9)) (concat oriented1)    

-- This shows that the tiles are all correctly aligned!
solve2alignment :: Map ID Tile -> String
solve2alignment tilesMap = makeTopRow tilesMap 2207
  & map (makeColumn tilesMap) & map (map fst)
  & map (map Mat.fromLists)
  -- & map (map (Mat.submatrix 2 9 2 9))
  & map (foldl1' (Mat.<->)) & foldl1' (Mat.<|>)
  & Mat.mapPos (\_ x -> if x then '#' else '.')
  & Mat.submatrix 1 30 1 30
  & Mat.prettyMatrix

solve2 :: Map ID Tile -> Mat.Matrix Bool
solve2 tilesMap = makeTopRow tilesMap 2207
  & map (makeColumn tilesMap) & map (map fst)
  & map (map Mat.fromLists)
  & map (map (Mat.submatrix 2 9 2 9))
  & map (foldl1' (Mat.<->)) & foldl1' (Mat.<|>)
  -- & Mat.mapPos (\_ x -> if x then '#' else '.')
  -- & Mat.submatrix 1 30 1 30
  -- & Mat.prettyMatrix

seaMonsters seaMon = [
  seaMon,
  reverse seaMon -- flip
  ] >>= \x -> [
    x,
    reverse . transpose $ x, -- rotations
    transpose . reverse $ x,
    reverse . map reverse $ x
  ]

findSeaMons mat sea = undefined
