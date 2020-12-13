{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- {-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

import Data.Functor ((<&>))
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Ord (comparing)
import Data.Function ((&))
import qualified Debug.Trace as Debug

main :: IO ()
main = do
  input <- readFile "13.txt" <&> lines
  let
    [ read @Int -> departFrom,
      splitOn "," -> mapMaybe (readMaybe @Int) -> schedules
      ]
      = input
  let
    [ _,
      splitOn "," -> map (readMaybe @Integer) -> schedules2
      ]
      = input

  let busId = solve departFrom schedules
  print busId
  let time = waitTime departFrom busId - 1
  print $ busId * time

  putStrLn "\n__Part 2"
  putStrLn "Sample: 67,7,59,61"
  print $ solve2result . scheduleOffsets . map Just $ [67,7,59,61]
  putStrLn "Sample: 67,x,7,59,61"
  print $ solve2result . scheduleOffsets $ [Just 67,Nothing,Just 7,Just 59,Just 61]
  putStrLn "Sample: 67,7,x,59,61"
  print $ solve2result . scheduleOffsets $ [Just 67,Just 7,Nothing,Just 59,Just 61]
  putStrLn "Sample: 1789,37,47,1889"
  print $ solve2result . scheduleOffsets . map Just $ [1789,37,47,1889]

  print $ scheduleOffsets schedules2
  print $ solve2result . scheduleOffsets $ schedules2

solve :: (Foldable t, Integral a) => a -> t a -> a
solve departFrom =
  minimumBy (comparing $ waitTime departFrom)

waitTime :: Integral a => a -> a -> a
waitTime departFrom freq =
  freq - (departFrom - 1) `mod` freq
  --- ! to offset for zero wait time

-- Part 2

-- Bus IDs must be co-prime for the problem to be solvable and non-trivial.

scheduleOffsets :: (Num t1, Enum t1) => [Maybe t2] -> [(t2, t1)]
scheduleOffsets schedules =
  zipWith (\x y -> x <&> (,y)) schedules [0..] & catMaybes

solve2result :: (Show p, Integral p) => [(p, p)] -> p
solve2result [] = 0
solve2result ((freq, _):xs) = solve2 0 freq xs

solve2 :: (Show t, Integral t) => t -> t -> [(t, t)] -> t
solve2 n _ [] = n
solve2 n m ((freq, offset):xs) =
  solve2 (findMatch n) (m*freq) xs & Debug.trace (show n)
  where
    findMatch n'
      | offset0 == (offset `mod` freq) = n'
        --- *** offset needs to be in mod freq !!!
        ---       Because if offset > freq then the above condition
        ---       will never match LOL.
        ---       Since offset0 is in mod freq.
      | otherwise = findMatch (n' + m) -- & Debug.trace (show n')
      where
        offset0 = waitTime n' freq - 1

-- Further explanation for Part 2:
{-
BusIDs were all prime, so one must simply add a multiple of the product of the previous BusIDs to maintain the same remainder mod all the previous BusIDs, such that the sum has a correct remainder mod the current BusID.

https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving
-}