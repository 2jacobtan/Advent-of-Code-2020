{-# LANGUAGE TypeApplications #-}

module Day1 where

runAnswer xs =
  take 1
    . map product
    . filter ((== 2020) . sum)
    $ do
      (x, i) <- zip xs [1 ..]
      x' <- drop i xs
      return [x, x']

runAnswer3 xs =
  take 1
    . map product
    . filter ((== 2020) . sum)
    $ do
      (x, i) <- zip xs [1 ..]
      (y, j) <- drop i xs `zip` [1 ..]
      z <- drop (i + j) xs
      return [x, y, z]

main = do
  -- inputFile <- openFile "input" ReadMode
  inputText <- readFile "input"
  print $ runAnswer . map (read @Int) . lines $ inputText
  print $ runAnswer3 . map (read @Int) . lines $ inputText


-- | Maths validation
-- >>> choose2 [1..200]
-- >>> choose3 [1..200]
-- 19900

-- 1313400

-- https://www.google.com/search?q=200+choose+2
-- 19900

-- https://www.google.com/search?q=200+choose+3
-- 1313400


choose2 :: [a] -> Int
choose2 xs =
  length
    $ do
      (x, i) <- zip xs [1 ..]
      x' <- drop i xs
      return [x, x']

choose3 :: [a] -> Int
choose3 xs =
  length
    $ do
      (x, i) <- zip xs [1 ..]
      (y, j) <- drop i xs `zip` [1 ..]
      z <- drop (i + j) xs
      return [x, y, z]


-- | StackOverflow solution

-- https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell/52605612

-- Every set contains a unique empty subset.
subsets 0 _ = [[]]

-- Empty sets don't have any (non-empty) subsets.
subsets _ [] = []

-- Otherwise we're dealing with non-empty subsets of a non-empty set.
-- If the first element of the set is x, we can get subsets of size n by either:
--   - getting subsets of size n-1 of the remaining set xs and adding x to each of them
--     (those are all subsets containing x), or
--   - getting subsets of size n of the remaining set xs
--     (those are all subsets not containing x)
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

chooseN n xs = length $ subsets n xs

-- >>> chooseN 2 [1..200]
-- >>> chooseN 3 [1..200]
-- 19900

-- 1313400
