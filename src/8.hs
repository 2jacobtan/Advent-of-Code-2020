{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Functor ((<&>))

data Instruction = Nop | Jmp Int | Acc Int

main :: IO ()
main = do
  input <- readFile "8.txt" <&> lines <&> map (parseInstruction . words)
  print $ solve1 . makeDict $ input

parseNum :: String -> Int
parseNum = \case
  ('+':xs) -> read @Int xs
  ('-':xs) -> - read @Int xs
  _ -> error "invalid parseNum"

parseInstruction :: [String] -> Instruction
parseInstruction = \case
  ["nop", _] -> Nop
  ["jmp", n] -> Jmp $ parseNum n
  ["acc", n] -> Acc $ parseNum n
  _ -> error "invalid parseInstruction"

makeDict :: [Instruction] -> Map Int Instruction 
makeDict = Map.fromList . zip [1 ..]

solve1 :: Map Int Instruction -> Int
solve1 dict = go Set.empty 0 1
  where
    go seen sum k
      | Set.member k seen = sum
      | otherwise =
          let
            (sumIncr, nextLine) =
              case dict Map.! k of
                Nop -> (id, k + 1)
                Jmp n -> (id, k + n)
                Acc n -> ((+n), k + 1)
          in go (Set.insert k seen) (sumIncr sum) nextLine
