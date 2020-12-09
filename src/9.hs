{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day9 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Sequence ((|>), Seq((:<|)))
import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
-- import qualified Debug.Trace as Debug


main :: IO ()
main = do
  input <- readFile "9.txt" <&> lines <&> map (read @Int)
  let part1sample = fromMaybe (-1) $ solve1 5 sample
  print part1sample
  let part1 = fromMaybe (-1) $ solve1 25 input
  print part1
  print $ solve2 part1sample sample
  let (x,y) = fromMaybe (-1,-1) $ solve2 part1 input
  print (x,y)
  print $ x+y

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
  foldr f (\_ _ -> Nothing) xs0 n S.empty
  where
    f x xs (lack::Int) (window::Seq Int) -- extra two parameters are both accumulators
      | x == lack =
          Just $ getSmallLarge (window |> x) -- & Debug.trace (show $ window |> x)
      | x > lack =
        case trimFront (x - lack) window of
          Left (net, residue) -> xs net (residue |> x) -- & Debug.trace (show $ residue |> x)
          Right residue ->
            Just $ getSmallLarge residue -- & Debug.trace (show residue)
      | x < lack = xs (lack - x) (window |> x) -- & Debug.trace (show $ window |> x)
      where
        getSmallLarge seqList = 
          (minimum seqList, maximum seqList) -- & Debug.trace (show seqList)

trimFront :: Int -> Seq Int -> Either (Int, Seq Int) (Seq Int)
trimFront (n::Int)
  | n < 0 = Left . (-n,)
  | n == 0 = Right -- exact trim means valid solution!
  | otherwise = \case
      a@S.Empty -> Left (n,a)
      (x:<|xs) -> trimFront (n - x) xs

-- >>> trimFront 3 $ S.fromList $ replicate 5 1
-- >>> trimFront 0 $ S.fromList $ replicate 5 1
-- >>> trimFront (-1) $ S.fromList $ replicate 5 1
-- Right (fromList [1,1])

-- Right (fromList [1,1,1,1,1])

-- Left (fromList [1,1,1,1,1])

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