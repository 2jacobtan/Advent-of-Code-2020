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
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (sortOn, foldl')
import qualified Debug.Trace as Debug
import Data.Functor ((<&>))
import qualified Data.IntMap as M
import Data.Monoid (Product(getProduct,Product))

dt t x = Debug.trace (t ++ ": " ++ show x) x
dt' x = Debug.trace (show x) x

main :: IO ()
main = do
  inputSample <- readFile "16sample.txt"
  inputSample2 <- readFile "16sample2.txt"
  inputRaw <- readFile "16.txt"
  -- parseTest rulesBlockP (pack inputRaw)
  -- parseTest (rulesBlockP *> yourTicketP) (pack inputRaw)
  -- parseTest (rulesBlockP *> yourTicketP *> nearbyTicketsP) (pack inputRaw)

  -- print $ solve1 . parseInput' $ inputSample
  print $ solve1 . parseInput' $ inputRaw

  putStrLn "\n__Part 2"
  -- print $ solve2 . parseInput' $ inputSample2
  -- print $ part2 . parseInput' $ inputSample2
  -- print $ part2' . parseInput' $ inputSample2
  print $ part2 . parseInput' $ inputRaw
  print $ part2' . parseInput' $ inputRaw

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

solve2 :: Input -> [[Int]]
solve2 Input{..} = nearbyTickets  -- & dt "nearbyTickets"
  & filter (isValidLine (map ruleToPred rulesBlock)) & (yourTicket :)
  & foldl' (flip filterListCandi) candidature
  where
    rulePredsV :: Vector (Int -> Bool)
    rulePredsV = V.fromList (map ruleToPred rulesBlock)

    rulesLen = V.length rulePredsV :: Int

    candidature :: [[Int]]
    candidature = replicate rulesLen [0..(rulesLen-1)]

    filterValCandi :: Int -> [Int] -> [Int]
    filterValCandi val candi = filter (\i -> rulePredsV V.! i $ val) candi

    filterListCandi :: [Int] -> [[Int]] -> [[Int]]
    filterListCandi list candiList = zipWith filterValCandi list candiList
      -- & dt "filterListCandi"

-- | (column#, field#) means: column# maps to field# in the field list
part2 :: (Num a, Enum a) => Input -> [(a, Set Int)]
part2 input = foldr f (const []) input' S.empty
  where
    input' = solve2 input <&> S.fromAscList & zip [0..] & sortOn (S.size . snd)
    f (i, candiList0) r assignedFields0 = (i, candiList1) : r assignedFields1
      where
        candiList1 = candiList0 S.\\ assignedFields0
        assignedFields1 = assignedFields0 <> candiList0

part2' :: Input -> Int
part2' input@Input{..} =
  getProduct $ foldMap (\j -> Product $ yourTicket !! j) relevantColumns
  where
    input' = part2 input  -- [(column#,[field#])]
    
    fieldToColMap = input' <&> (\(c,S.elems -> (f:_)) -> (f,c)) & M.fromList
    
    -- | field#s
    relevantRules = zip [0..] rulesBlock & filter (isDeparture . fst . snd)
    isDeparture x = take (length @[] "departure") x == "departure"
    
    relevantColumns = map (\(fst -> i) -> fieldToColMap M.! i) relevantRules
