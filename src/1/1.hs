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
