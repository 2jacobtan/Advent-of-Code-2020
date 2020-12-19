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
import Text.Megaparsec (try, sepBy1, (<|>), parseTest, parseMaybe,  many, sepBy, some, Parsec )
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Char.Lexer (lexeme, decimal)
import Text.Megaparsec.Char (eol, hspace, letterChar, string)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import qualified Data.Set as S
import Data.List.Extra (sumOn')

main = do
  -- inputRaw <- readFile "19.txt"
  -- parseTest inputP inputRaw
  (rules, messages) <- readFile "19.txt" <&> parseMaybe inputP <&> fromMaybe (error "failParse")
  
  (rulesSample, _) <- readFile "19sample.txt" <&> parseMaybe inputP <&> fromMaybe (error "failParse")

  print $ mkRule [[0]] . mkRuleMap $ rulesSample

  putStrLn "\n__Part 1"
  -- mapM_ print rules
  -- print $ mkRule [[0]] . mkRuleMap $ rules
  print $ part1 rules messages


-- Parsing 

type Parser = Parsec Void String

data Rule = Ref [[Int]] | Val Char
  deriving Show

inputP :: Parser (
  [(Int, Rule)],
  [String]
  )
inputP = (,) <$> some (ruleP <* eol)
  <* eol
  <*> sepBy (some letterChar) eol

ruleP :: Parser (Int, Rule)
ruleP = do
  (,) <$> numP <* ": " <*> (
    Ref <$> sepBy1 (some numP) "| "
    <|> Val <$ "\"" <*> letterChar <* "\""
    )

numP :: Parser Int
numP = lexeme hspace decimal


-- Part 1

mkRuleMap :: [(Int, Rule)] -> IntMap Rule
mkRuleMap = M.fromList

mkRule :: [[Int]] -> IntMap Rule -> [String]
mkRule iss ruleMap = go iss
  where
    go :: [[Int]] -> [[Char]]
    go xss = concatMap f xss
      where
        f :: [Int] -> [[Char]]
        f (x:xs) = case ruleMap M.! x of
          Ref yss -> go $ map (++ xs) yss
          Val c -> map (c:) (f xs)
        f [] = [[]]

solve1 :: Num b => [(Int, Rule)] -> [String] -> b
solve1 rules messages = messages
  & sumOn' (\x -> if S.member x validMessages then 1 else 0)
  where 
    validMessages = rules
      & mkRule [[0]] . mkRuleMap
      & S.fromList
{- HLINT ignore "Eta reduce" -}

part1 :: Num b => [(Int, Rule)] -> [String] -> b
part1 = solve1


-- Part 2

solve2 :: Num b => [(Int, Rule)] -> [String] -> b
solve2 rules messages = messages
  & sumOn' (\x -> if S.member x validMessages then 1 else 0)
  where 
    validMessages = rules
      & mkRuleMap
      & M.insert 8 (Ref [[42],[42,8]])
      & M.insert 11 (Ref [[42,31],[42,11,31]])
      & mkRule [[0]] 
      & S.fromList
{- HLINT ignore "Eta reduce" -}
