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

main = do
  inputRaw <- readFile "19.txt"
  parseTest inputP inputRaw
  -- input <- readFile "20.txt" <&> parseMaybe inputP <&> fromMaybe (error "failParse")
  -- print input

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

