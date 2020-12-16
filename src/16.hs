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

import Text.Megaparsec (parseMaybe, sepBy, parseTest, anySingle, manyTill, Parsec)
import Data.Text (pack, Text)
import Data.Void (Void)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  inputSample <- readFile "16sample.txt"
  inputRaw <- readFile "16.txt"
  -- parseTest rulesBlockP (pack inputRaw)
  -- parseTest (rulesBlockP *> yourTicketP) (pack inputRaw)
  -- parseTest (rulesBlockP *> yourTicketP *> nearbyTicketsP) (pack inputRaw)

  -- print $ solve1 . parseInput' $ inputSample
  print $ solve1 . parseInput' $ inputRaw

  putStrLn "\n__Part 2"
  print $ solve2 . parseInput' $ inputSample

-- Parsing

type Parser = Parsec Void Text

rulesBlockP :: Parser [(String, (T2Int, T2Int))]
rulesBlockP = manyTill rulesLineP eol

type T2Int = (Int,Int)

rulesLineP :: Parser (String,(T2Int,T2Int))
rulesLineP = (\k v@(_v1,_v2) w@(_w1,_w2) -> (k, (v,w)))
  <$> manyTill anySingle ": "
  <*> ((,) <$> decimal <* "-" <*> decimal)
  <* " or "
  <*> ((,) <$> decimal <* "-" <*> decimal)
  <* eol

yourTicketP :: Parser [Int]
yourTicketP = "your ticket:" *> eol
  *> sepBy decimal "," <* eol <* eol

nearbyTicketsP :: Parser [[Int]]
nearbyTicketsP = "nearby tickets:" *> eol
  *> sepBy (sepBy decimal ",") "\n"

data Input = Input {
  rulesBlock :: [(String, (T2Int, T2Int))],
  yourTicket :: [Int],
  nearbyTickets :: [[Int]]
  }

parseInput :: Parser Input
parseInput = Input <$> rulesBlockP <*> yourTicketP <*> nearbyTicketsP

parseInput' :: String -> Input
parseInput' raw = fromMaybe (error "bad parse") $ parseMaybe parseInput (pack raw)

-- Part 1

inRange :: Ord a => a -> a -> a -> Bool
inRange a b x = a <= x && x <= b

ruleToPred :: Ord a1 => (a2, ((a1, a1), (a1, a1))) -> a1 -> Bool
ruleToPred (_, ((v1,v2),(w1,w2))) = (||) <$> inRange v1 v2 <*> inRange w1 w2

meetSomeRule :: Foldable t => t (a -> Bool) -> a -> Bool
meetSomeRule predRules val = any ($ val) predRules 

invalidVals :: Foldable t => t (a -> Bool) -> [a] -> [a]
invalidVals predRules line = filter (not . meetSomeRule predRules) line
{- HLINT ignore "Eta reduce" -}

solve1 :: Input -> Int
solve1 Input{..} = nearbyTickets
  & map (invalidVals (map ruleToPred rulesBlock))
  & map sum & sum


-- Part 2

isValidLine :: (Foldable t1, Foldable t2) => t2 (a -> Bool) -> t1 a -> Bool
isValidLine predRules line = all (meetSomeRule predRules) line



solve2 Input{..} = nearbyTickets
  & filter (isValidLine (map ruleToPred rulesBlock)) & (yourTicket :)
  & id
  -- & foldl' f (S.fromList )

