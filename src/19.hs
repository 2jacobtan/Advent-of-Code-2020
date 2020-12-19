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

main = do
  -- inputRaw <- readFile "19.txt"
  -- parseTest inputP inputRaw
  input <- readFile "19.txt" <&> parseMaybe inputP <&> fromMaybe (error "failParse")
  
  (rules, _) <- readFile "19sample.txt" <&> parseMaybe inputP <&> fromMaybe (error "failParse")

  print $ mkRule [[0]] . mkRuleMap $ rules
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

-- mkRule :: [[Int]] -> IntMap Rule -> [String]
-- mkRule iss ruleMap = go2 $ go iss
--   where
--     go :: [[Int]] -> [[Int]]
--     go xss = do
--       xs <- xss  -- each possible candidate
--       foldr f [] xs
--       where
--         f x r =
--           let
--             yss = case ruleMap M.! x of
--               Ref yss' -> go yss'
--               Val _ -> [[x]]
--           in do
--               ys <- yss
--               return ys ++ r
            
--     go2 :: [[Int]] -> [String]
--     go2 xss = map (map f) xss
--       where
--         f x = case ruleMap M.! x of
--           Val c -> c
--           Ref _ -> error "unexpected ref"
