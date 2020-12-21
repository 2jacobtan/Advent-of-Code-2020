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
import Text.Megaparsec ( parseMaybe, sepBy1, some, Parsec )
import Text.Megaparsec.Char ( eol, hspace, letterChar )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Set (Set)
import Data.Map (Map)
import Control.Monad.Trans.State.Strict (execState, put, get, State)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Parser = Parsec Void String

-- Parsing

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

lineP :: Parser (Ingredients, Allergens)
lineP = (\x y -> (M.fromListWith (+) (map (,1) x),S.fromList y))
  <$> some (lexeme (some letterChar)) <* "(contains "
  <*> sepBy1 (some letterChar) ", " <* ")"

inputP :: Parser [(Ingredients, Allergens)]
inputP = sepBy1 lineP eol


-- main

main :: IO ()
main = do
  inputRaw <- readFile "21.txt"
  -- parseTest inputP inputRaw
  let input = fromMaybe (error "failParse") $ parseMaybe inputP inputRaw
  (\(x,y) -> print x >> putStrLn "\n" >> print y) $ solve1 input

  print $ part1 input

  putStrLn "\n__Part 2"
  print $ solve2 input
  part2 input >> putStrLn "\n"


-- Part 1
type Ingredients = Map String Int
type Allergens = Set String
type AllergensM = Map String Allergens

eachLine :: (Ingredients, Allergens) -> State (Ingredients, AllergensM) ()
eachLine (ingred, allerg) = do
  (ingred0, allergM0) <- get
  let allergM = M.fromSet (const (M.keysSet ingred)) allerg
  let allergM1 = M.unionWith S.intersection allergM0 allergM
  let ingred1 = M.unionWith (+) ingred0 ingred
  put (ingred1, allergM1)

solve1 :: [(Ingredients, Allergens)] -> (Ingredients, AllergensM)
solve1 inputList = execState (mapM_ eachLine inputList) (M.empty, M.empty)

part1 :: [(Ingredients, Allergens)] -> Int
part1 inputList =
  let
    (ingred, allergM) = solve1 inputList
    ingredNonAllerg = M.withoutKeys ingred (allergM & M.elems & S.unions)
  in 
    sum ingredNonAllerg


-- Part 2

solve2 :: [(Ingredients, Allergens)] -> AllergensM
solve2 inputList = go definite_ambiguous0
  where
    (_, allergM) = solve1 inputList
    definite_ambiguous0 = M.partition (\x -> S.size x == 1) allergM
    go (definite, ambiguous) = case M.size ambiguous of
      0 -> definite
      _ -> go definite_ambiguous1
      where
        (definite1, ambiguous1) = M.map eliminateDet ambiguous
          & M.partition (\x -> S.size x == 1)
        eliminateDet amb0 = foldl' (\amb def -> amb S.\\ def) amb0 definite
        definite_ambiguous1 = (M.union definite definite1, ambiguous1)

part2 :: [(Ingredients, Allergens)] -> IO ()
part2 inputList = solve2 inputList
  & M.toAscList & concatMap (S.toList . snd) & mapM_ (\x -> putStr (x ++ ","))

