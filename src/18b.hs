-- Part 2
-- abandoned failed attempt

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

import Data.Foldable (Foldable(foldl'))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Text.Megaparsec (runParser, parseMaybe, parseTest, between, try, Parsec)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (pack, Text)
import Data.Void (Void)
import Control.Applicative (Alternative(many, (<|>)))
import Data.Maybe (fromMaybe)
import Control.Monad (void)

main :: IO ()
main = do
  input <- readFile "18.txt" <&> lines
  let s1 = "2 * 3 + (4 * 5)"
  parseTest exprP (pack s1)
  print $ evalParse exprP s1
  let s1 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"
  parseTest exprP (pack s1)
  print $ runParser exprP "" (pack s1)
  print $ evalParse exprP s1

  -- putStrLn "\n__head test"
  -- parseTest exprP (pack $ head input)
  -- print $ evalParse exprP (head input)
  -- print $ part2 input

type Parser = Parsec Void Text

evalParse :: Parser Expr -> String -> Int
evalParse p = eval . fromMaybe (error "failParse") . parseMaybe p . pack

data UnitExpr
  = Num Int
  | Paren Expr
  deriving Show

data Expr
  = Expr UnitExpr [SubExpr]
  | ExprExpr2 Expr2
  deriving Show

newtype SubExpr
  = Mul Expr2
  deriving Show

data Expr2 = Expr2 UnitExpr [SubExpr2]
  deriving Show

newtype SubExpr2
  = Add UnitExpr
  deriving Show

evalUnit :: UnitExpr -> Int
evalUnit = \case
  Num x -> x
  Paren e -> eval e

eval :: Expr -> Int
eval (Expr head tail) = foldl' f (evalUnit head) tail
  where
    f l = \case
      Mul x -> l * eval2 x
eval (ExprExpr2 e) = eval2 e

eval2 :: Expr2 -> Int
eval2 (Expr2 head tail) = foldl' f (evalUnit head) tail
  where
    f l = \case
      Add x -> l + evalUnit x

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

addP :: Parser SubExpr2
addP = Add <$ symbol "+" <*> unitP

mulP :: Parser SubExpr
mulP = Mul <$ symbol "*" <*> expr2P

subExprP :: Parser SubExpr
subExprP = mulP

exprP :: Parser Expr
exprP = try (Expr <$> unitP <*> many subExprP)
  <|> ExprExpr2 <$> expr2P

subExpr2P :: Parser SubExpr2
subExpr2P = addP

expr2P :: Parser Expr2
expr2P = Expr2 <$> unitP <*> many subExpr2P

part2 :: [String] -> Int
part2 = sum . map (evalParse exprP)
