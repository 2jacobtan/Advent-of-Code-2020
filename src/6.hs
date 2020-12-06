import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.List (foldl1')
import Data.List.Split (splitOn)
import qualified Data.Set as Set

solve :: (Foldable t, Ord a) => [t [a]] -> Int
solve =
  map (concat >>> Set.fromList >>> Set.size)
    >>> sum

solve2 :: Ord a => [[[a]]] -> Int
solve2 =
  map (map Set.fromList >>> foldl1' Set.intersection >>> Set.size)
    >>> sum

main :: IO ()
main = do
  input <- readFile "6.txt" <&> splitOn "\n\n" <&> map lines
  print $ solve input
  print $ solve2 input
