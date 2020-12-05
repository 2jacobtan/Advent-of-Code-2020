{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

import Data.Function ((&))
import Data.List (sort)

rowIndex :: String -> Int
rowIndex letters = letters
  & map (\case 'F' -> 0; _ -> 1) 
  & zipWith (\x y -> 2^x * y) [0..] . reverse
  & sum

colIndex :: String -> Int
colIndex letters = letters
  & map (\case 'L' -> 0; _ -> 1) 
  & zipWith (\x y -> 2^x * y) [0..] . reverse
  & sum

answer :: String -> Int
answer (splitAt 7 -> (row,col)) =
  rowIndex row * 8 + colIndex col

main :: IO ()
main = do
  inputLines <- lines <$> readFile "5.txt"
  print $ maximum . map answer $ inputLines
  print $
    (\(x:xs) -> foldr (\y ys k -> if k+2 == y then k+1 else ys y) undefined xs x )
      . sort . map answer $ inputLines
  