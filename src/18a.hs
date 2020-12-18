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

main :: IO ()
main = do
  input <- readFile "18.txt" <&> lines
  print $ evalParse exprP "2 * 3 + (4 * 5)"
  parseTest exprP (pack $ head input)
  print $ evalParse exprP (head input)
  print $ part1 input

type Parser = Parsec Void Text

evalParse :: Parser Expr -> String -> Int
evalParse p = eval . fromMaybe (error "failParse") . parseMaybe p . pack

data Expr = Expr UnitExpr [SubExpr]
  deriving Show

data UnitExpr
  = Num Int
  | Paren Expr
  deriving Show

data SubExpr
  = Add UnitExpr
  | Mul UnitExpr
  deriving Show

evalUnit :: UnitExpr -> Int
evalUnit = \case
  Num x -> x
  Paren e -> eval e

eval :: Expr -> Int
eval (Expr head tail) = foldl' f (evalUnit head) tail
  where
    f l = \case
      Add x -> l + evalUnit x
      Mul x -> l * evalUnit x

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

part1 :: [String] -> Int
part1 = sum . map (evalParse exprP)
