{-# LANGUAGE TypeApplications #-}

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
