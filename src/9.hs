{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day9 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Sequence ((|>), Seq((:<|),(:|>)))
import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
import qualified Debug.Trace as Debug


main :: IO ()
main = do
  input <- readFile "9.txt" <&> lines <&> map (read @Int)
  let part1 = fromMaybe (-1) $ solve1 25 input
  print part1
  let part1sample = fromMaybe (-1) $ solve1 5 sample
  print part1sample
  -- print $ solve2 part1 input
  print $ solve2 part1sample sample

-- | generate 2-combinations from a list
choose2 :: Seq a -> Seq [a]
choose2 xs0 = do
  xss0 <- S.tails xs0
  case xss0 of
    (x :<| xs) -> do
      y <- xs
      return [x, y]
    _ -> S.empty

solve1 :: Int -> [Int] -> Maybe Int
solve1 n (splitAt n -> (preamble, rest)) =
  foldr f (const Nothing) rest (S.fromList preamble)
  where
    f x xs ys25 = -- 3rd argument acts as an accumulator
      case S.filter (==x) (pairSums ys25) of
        S.Empty -> Just x
        _ -> xs (ys25 & S.drop 1 & (|> x))

pairSums :: Seq Int -> Seq Int
pairSums some25 = sum <$> choose2 some25

-- Part 2

solve2 :: Int -> [Int] -> Maybe (Int,Int)
solve2 n xs0 =
  foldr f (const Nothing) xs0 S.empty
  where
    f x xs accum
      | totalSum == n =
          getFirstLast accum
      | totalSum > n =
        case trimFront (totalSum - n) accum of
          Left residue -> xs (residue |> x)
          Right residue ->
            getFirstLast residue
      | totalSum < n = xs $ accum |> x
      where
        totalSum = Debug.trace (show accum) $ sum accum
        getFirstLast seqList = 
          let (start:<|_) = seqList; (_:|>stop) = seqList
          in Just (start, stop)

trimFront :: Int -> Seq Int -> Either (Seq Int) (Seq Int)
trimFront (n::Int)
  | n < 0 = Left
  | n == 0 = Right
  | otherwise = \case
      a@S.Empty -> Left a
      (x:<|xs) -> trimFront (n - x) xs

-- >>> trimFront 3 $ S.fromList $ replicate 5 1
-- >>> trimFront 0 $ S.fromList $ replicate 5 1
-- >>> trimFront (-1) $ S.fromList $ replicate 5 1
-- fromList [1,1]

-- fromList [1,1,1,1,1]

-- fromList [1,1,1,1,1]

sample =
  [
    35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ]