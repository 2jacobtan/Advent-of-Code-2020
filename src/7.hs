{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Functor ((<&>))
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(Sum, getSum))
import Data.Function ((&))

main :: IO ()
main = do
  input <- readFile "7.txt" <&> lines <&> map words
  print $ length . satisfy "shiny gold" . Map.fromListWith Set.union . concatMap parseLine $ input
  print $ countBags "shiny gold" . Map.fromList . map parseLine2 $ input

parseLine :: [String] -> [(String, Set String)]
parseLine
  (splitAt 2 ->
    ( unwords -> container,
      drop 2 -> chunksOf 4 -> contentsRaw
    )
  ) 
  = [(inner, Set.fromList [container]) | (_, inner) <- contents]
  -- e.g. container == "shiny gold"
  -- e.g. contents == [("1", "bright white"), ("2", "muted yellow")]
    where
      contents = map (fmap unwords . splitAt 1 . take 3) contentsRaw

satisfy :: String -> Map String (Set String) -> Set String
satisfy start dict
  = go Set.empty Set.empty [start]
  where
    go _ answers [] = answers
    go seen answers (x:xs) =
      let
        nexts = fromMaybe Set.empty (Map.lookup x dict) `Set.difference` seen
      in
        go
          (Set.union seen nexts)
          (Set.union answers nexts)
          (xs ++ Set.toList nexts)


-- Part 2

parseLine2 :: [String] -> (String, [(Int, String)])
parseLine2
  (splitAt 2 ->
    ( unwords -> container,
      drop 2 -> chunksOf 4 -> contentsRaw
    )
  ) 
  = (container, map parseContent contents)
  -- e.g. container == "shiny gold"
  -- e.g. contents == [("1", "bright white"), ("2", "muted yellow")]
    where
      contents = map (fmap unwords . splitAt 1 . take 3) contentsRaw
      parseContent (head -> readNum -> n, inner) = (n, inner)
      readNum = \case
        "no" -> 0
        x -> read @Int x

countBags :: String -> Map String [(Int, String)] -> Int
countBags start dict = go start & getSum
  where
    inners outer = fromMaybe [] (Map.lookup outer dict)
    -- go bag = sum . map (\(n, innerBag) -> n + n * go innerBag) . inners $ bag
    --- | alternative version using foldMap
    go bag = foldMap (\(Sum -> n, innerBag) -> n + n * go innerBag) . inners $ bag

  