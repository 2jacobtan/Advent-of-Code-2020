{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Instruction = Nop Int | Jmp Int | Acc Int

main :: IO ()
main = do
  input <- readFile "8.txt" <&> lines <&> map (parseInstruction . words)
  print $ solve1 . makeDict $ input

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
    lastLine = Map.size dict
    go seen sum k
      | Set.member k seen = Left sum -- on reaching previously seen line
      | otherwise = case Map.lookup k dict of
        Nothing -> Left sum -- on jumping out of valid range of line numbers
        Just val ->
          let (sumIncr, nextLine) =
                case val of
                  Nop _ -> (id, k + 1)
                  Jmp n -> (id, k + n)
                  Acc n -> ((+ n), k + 1)
          in go (Set.insert k seen) (sumIncr sum) nextLine

solve2 dict = undefined
  where
    possibilities = Map.mapMaybeWithKey f dict
    f k = \case
      Nop n -> Just $ Map.insert k (Jmp n)
      Jmp n -> Just $ Map.insert k (Nop n)
      Acc _ -> Nothing
