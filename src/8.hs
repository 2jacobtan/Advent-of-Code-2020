{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

data Instruction = Nop Int | Jmp Int | Acc Int

main :: IO ()
main = do
  input <- readFile "8.txt" <&> lines <&> map (parseInstruction . words)
  print $ solve1 . makeDict $ input
  print $ solve2result . makeDict $ input

parseNum :: String -> Int
parseNum = \case
  ('+' : xs) -> read @Int xs
  ('-' : xs) -> - read @Int xs
  _ -> error "invalid parseNum"

parseInstruction :: [String] -> Instruction
parseInstruction = \case
  ["nop", n] -> Nop $ parseNum n
  ["jmp", n] -> Jmp $ parseNum n
  ["acc", n] -> Acc $ parseNum n
  _ -> error "invalid parseInstruction"

makeDict :: [Instruction] -> Map Int Instruction
makeDict = Map.fromList . zip [1 ..]

solve1 :: Map Int Instruction -> Either Int Int
solve1 dict = go Set.empty 0 1
  where
    go seen !sum !k
      | Set.member k seen = Right sum -- on reaching previously seen line
      | otherwise = case Map.lookup k dict of
        Nothing -> Left sum -- on jumping out of valid range of line numbers
        Just val ->
          let (sumIncr, nextLine) =
                case val of
                  Nop _ -> (id, k + 1)
                  Jmp n -> (id, k + n)
                  Acc n -> ((+ n), k + 1)
          in go (Set.insert k seen) (sumIncr sum) nextLine


-- Part 2

solve2 :: Map Int Instruction -> Maybe Int
solve2 dict = go Set.empty 0 1
  where
    lastLine = Map.size dict -- for part 2
    go seen !sum !k
      | Set.member k seen = Nothing -- on reaching previously seen line
      | otherwise = case Map.lookup k dict of
        Nothing -> Nothing -- on jumping out of valid range of line numbers
        Just val ->
          let (sumIncr, nextLine) =
                case val of
                  Nop _ -> (id, k + 1)
                  Jmp n -> (id, k + n)
                  Acc n -> ((+ n), k + 1)
              newSum = sumIncr sum
          in
            -- recognise valid end condition and exit loop
            if k == lastLine then case val of
              Nop _ -> Just newSum
              Acc _ -> Just newSum
              Jmp 1 -> Just newSum
              Jmp _ -> Nothing -- invalid if jump back or jump forward >1
            else go (Set.insert k seen) newSum nextLine

solve2result :: Map Int Instruction -> [Int]
solve2result dict = mapMaybe solve2 possibilities
  where
    possibilities = Map.elems $ Map.mapMaybeWithKey f dict
    f k = \case
      Nop n -> Just $ Map.insert k (Jmp n) dict
      Jmp n -> Just $ Map.insert k (Nop n) dict
      Acc _ -> Nothing
