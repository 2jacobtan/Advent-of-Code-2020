{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import qualified Data.Set as S
import Data.List.Extra (sumOn')
import Data.Set (Set)

main = do
  -- inputRaw <- readFile "19.txt"
  -- parseTest inputP inputRaw
  (rules, messages) <- readFile "19.txt" <&> parseMaybe inputP <&> fromMaybe (error "failParse")
  
  -- (rulesSample, _) <- readFile "19sample.txt" <&> parseMaybe inputP <&> fromMaybe (error "failParse")

  -- print $ mkRule [[0]] . mkRuleMap $ rulesSample

  putStrLn "\n__Part 1"
  -- mapM_ print rules
  -- print $ mkRule [[0]] . mkRuleMap $ rules
  
  -- print $ part1 rules messages  -- [[0]]
  -- print $ part1b rules messages  -- [[8,11]]
  -- print $ part1c rules messages  -- [[42,42,31]]
    -- == 136
  
  putStrLn "\n__Part 2"
  putStrLn "\nRule42"
  let s42 = S.fromList $ mkRule [[42]] . mkRuleMap $ rules
  print s42
  putStrLn "\nRule31"
  let s31 = S.fromList $ mkRule [[31]] . mkRuleMap $ rules
  print s31
  putStrLn $ "S.intersection s42 s31: " ++ show (S.intersection s42 s31)
  putStrLn $ "S.map length s42: " ++ show (S.map length s42)
  putStrLn $ "S.map length s31: " ++ show (S.map length s31)
  print $ part2 rules messages


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

solve1 :: Num b => Set String -> [(Int, Rule)] -> [String] -> b
solve1 validMessages rules messages = messages
  & sumOn' (\x -> if S.member x validMessages then 1 else 0)
{- HLINT ignore "Eta reduce" -}

part1 :: Num b => [(Int, Rule)] -> [String] -> b
part1 rules = solve1 (rules & mkRule [[0]] . mkRuleMap & S.fromList) rules
part1b rules = solve1 (rules & mkRule [[8,11]] . mkRuleMap & S.fromList) rules
part1c rules = solve1 (rules & mkRule [[42,42,31]] . mkRuleMap & S.fromList) rules


-- Part 2

-- Requirement:
--   repeat 42 N times then repeat 31 M times, N > M, M>=1.
-- Data characteristics:
--   rule42 does not overlap with rule31
--   each candidate in 42 or 31 has length 8

solve2 :: [(Int, Rule)] -> [String] -> Int
solve2 rules messages = length . mapMaybe tryMatch $ messages
  where 
    rule42 = rules
      & mkRuleMap
      & mkRule [[42]] 
      & S.fromList
    rule31 = rules
      & mkRuleMap
      & mkRule [[31]] 
      & S.fromList
    
    tryMatch :: String -> Maybe ()
    tryMatch xs = let (n,rest) = tryMatch42 xs 0
      in case tryMatch31 rest 0 of
          Just (m,_) -> if n > m && m >=1 then Just () else Nothing
          -- Just (m,_) -> if n == 2 && m == 1 then Just () else Nothing  -- 136
            -- this gives Part 1 answer
          Nothing -> Nothing
    tryMatch42 :: String -> Int -> (Int, String)
    tryMatch42 [] n = (n,[])
    tryMatch42  (splitAt 8 -> (start,rest)) n
      | S.member start rule42 = tryMatch42 rest (n+1)
      | otherwise = (n,start++rest)
    tryMatch31 :: String -> Int -> Maybe (Int, String)
    tryMatch31 [] m = Just (m,[])
    tryMatch31  (splitAt 8 -> (start,rest)) m
      | S.member start rule31 = tryMatch31 rest (m+1)
      | otherwise = Nothing

part2 :: [(Int, Rule)] -> [String] -> Int
part2 = solve2
