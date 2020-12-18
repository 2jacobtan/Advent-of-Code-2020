-- Part 1

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

import Data.Foldable (Foldable(foldl'))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Text.Megaparsec (parseMaybe, parseTest, between, try, Parsec)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (pack, Text)
import Data.Void (Void)
import Control.Applicative (Alternative(many, (<|>)))
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

main :: IO ()
main = do
  input <- readFile "18.txt" <&> lines
  print $ evalParse exprP "2 * 3 + (4 * 5)"
  parseTest exprP "5 + (8 * 3 + 9 + 3 * 4 * 3)"
  print $ evalParse exprP "5 + (8 * 3 + 9 + 3 * 4 * 3)"
  parseTest exprP "8 * 3 + 9 + 3 * 4 * 3"
  print $ evalParse exprP "8 * 3 + 9 + 3 * 4 * 3"
  print $ evalParse exprP "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
  print $ evalParse exprP "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
  
  putStrLn "__Part 2 answer"
  print $ part1 input

type Parser = Parsec Void Text

evalParse :: Parser Expr -> String -> Integer
evalParse p = eval . fromMaybe (error "failParse") . parseMaybe p . pack

data Expr = Expr UnitExpr [SubExpr]
  deriving Show

data UnitExpr
  = Num Integer
  | Paren Expr
  deriving Show

data SubExpr
  = Add UnitExpr
  | Mul UnitExpr
  deriving Show

evalUnit :: UnitExpr -> Integer
evalUnit = \case
  Num x -> x
  Paren e -> eval e

eval :: Expr -> Integer
eval (Expr head tail) = go (evalUnit head) tail
  where
    go :: Integer -> [SubExpr] -> Integer
    go x [] = x
    go x (y_:ys) = case y_ of
      Add y -> go (x + evalUnit y) ys
      Mul y -> x * go (evalUnit y) ys

aoeu = "(8 * 3 + 9 + 3 * 4 * 3)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

numP :: Parser UnitExpr
numP = lexeme L.decimal <&> Num

parenExprP :: Parser UnitExpr
parenExprP = Paren <$> parens exprP

unitP :: Parser UnitExpr
unitP = numP <|> parenExprP

addP :: Parser SubExpr
addP = Add <$ symbol "+" <*> unitP

mulP :: Parser SubExpr
mulP = Mul <$ symbol "*" <*> unitP

subExprP :: Parser SubExpr
subExprP = addP <|> mulP

exprP :: Parser Expr
exprP = Expr <$> unitP <*> many subExprP

part1 :: [String] -> Integer
part1 = sum . map (evalParse exprP)
