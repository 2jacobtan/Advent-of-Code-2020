{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.Bits (Bits (xor))
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

isValid :: (Int, Int, Char, Text) -> Bool
isValid (min, max, char, password) =
  (\x -> min <= x && x <= max)
    (T.length . T.filter (== char) $ password)

isValid2 :: (Int, Int, Char, Text) -> Bool
isValid2 (p1, p2, char, password) =
  let pwLength = T.length password
      test1 = case p1 <= pwLength of
        True -> T.index password (p1-1) == char
        False -> False
      test2 = case p2 <= pwLength of
        True -> T.index password (p2-1) == char
        False -> False
   in test1 `xor` test2

countValids :: [(Int, Int, Char, Text)] -> Int
countValids = length . filter isValid

countValids2 :: [(Int, Int, Char, Text)] -> Int
countValids2 = length . filter isValid2

parserLine :: Parser (Int, Int, Char, Text)
parserLine = do
  min <- some digitChar
  char '-'
  max <- some digitChar
  space1
  char <- lowerChar
  string ": "
  password <- some lowerChar
  return (read min, read max, char, pack password)

main = do
  inputText <- readFile "input"
  let input =
        either (const (-1, -1, '!', "!!")) id
          . (parse parserLine "")
          <$> (T.lines (pack inputText))

  print $ countValids input
  print $ countValids2 input
