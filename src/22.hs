{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

import Data.Foldable (Foldable(toList, foldl'))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Data.List.Extra (stripInfix)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Q
import Data.Sequence ((|>), (<|), Seq(..))
import qualified Data.Set as S
import Control.Monad.Trans.State.Strict (evalState, State, put, get)
import Data.Set (Set)

main :: IO ()
main = do
  inputRaw <- readFile "22.txt"
  let (fromMaybe ("","") . stripInfix "\n\n" ->
        (lines -> drop 1 -> map read -> player1,
        lines -> drop 1 -> map read -> player2)) = inputRaw

  inputRawSample <- readFile "22sample.txt"
  let (fromMaybe ("","") . stripInfix "\n\n" ->
        (lines -> drop 1 -> map read -> player1sample,
        lines -> drop 1 -> map read -> player2sample)) = inputRawSample
  
  print player1
  print player2
  putStrLn "\n__Part 1"
  print $ part1 player1 player2

  putStrLn "\n__Part 2 sample"
  print $ part2 player1sample player2sample
  
  putStrLn "\n__Part 2"
  print $ part2 player1 player2

-- Part 1

score :: (Num a, Foldable t, Enum a) => t a -> a
score cards = sum $ zipWith (*) (toList cards) [1..]

solve1 :: (Num a, Enum a, Ord a) => Seq a -> Seq a -> a
solve1 p1 Q.Empty  = score p1
solve1 Q.Empty p2 = score p2
solve1 (xs:|>x) (ys:|>y)
  | x > y = solve1 (y<|x<|xs) ys
  | x < y = solve1 xs (x<|y<|ys)
solve1 _ _ = error "no such possibility"

part1 :: (Num a, Enum a, Ord a) => [a] -> [a] -> a
part1 (reverse -> Q.fromList -> p1) (reverse -> Q.fromList -> p2) = solve1 p1 p2


-- Part 2

score2 :: Seq Int -> Int
score2 (Q.reverse -> cards) = sum $ zipWith (*) (toList cards) [1..]

solve2 :: Seq Int -> Seq Int -> State (Set (Seq Int, Seq Int)) (Bool, Int)
solve2 p1 Q.Empty  = return (True, score2 p1)
solve2 Q.Empty p2 = return (False, score2 p2)
solve2 p1@(x:<|xs) p2@(y:<|ys) = do
  history <- get
  if S.member (p1,p2) history
  then return (True, score2 p1)  -- True means p1 is winner
  else do
    put $ S.insert (p1,p2) history
    if Q.length xs >= x && Q.length ys >= y
    then case evalState (solve2 (Q.take x xs) (Q.take y ys)) S.empty of
      (True, _) -> solve2 (xs|>x|>y) ys
      (False, _) -> solve2 xs (ys|>y|>x)
    else if
      | x > y -> solve2 (xs|>x|>y) ys
      | x < y -> solve2 xs (ys|>y|>x)
      | otherwise -> error "no such possibility"

part2 :: [Int] -> [Int] -> (Bool, Int)
part2 (Q.fromList -> p1) (Q.fromList -> p2) =
  evalState (solve2 p1 p2) S.empty
