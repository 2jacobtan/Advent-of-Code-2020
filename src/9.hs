{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Sequence ((|>), Seq((:<|)))
import qualified Data.Sequence as S

main :: IO ()
main = do
  input <- readFile "9.txt" <&> lines <&> map (read @Int)
  print $ solve1 25 input
  -- print $ solve1 5 sample

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
    f x xs ys25 =
      case S.filter (==x) (pairSums ys25) of
        S.Empty -> Just x
        _ -> xs (ys25 & S.drop 1 & (|> x))

pairSums :: Seq Int -> Seq Int
pairSums some25 = sum <$> choose2 some25

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