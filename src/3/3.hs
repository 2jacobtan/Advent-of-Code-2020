{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Direction = Dir {right :: Int, down :: Int}

solve :: [[Char]] -> Direction -> Int
solve rows Dir {right, down} = go 0 0 rows
  where
    go n i = \case
      [] -> n -- total trees
      xxs@(x : _) ->
        go
          (incrementFun n) -- update total trees
          ((i + right) `mod` width) -- go right
          (drop down xxs) -- go down
        where
          incrementFun = if x !! i == '.' then id else (+ 1)
    width = case take 1 rows of
      x : _ -> length x
      [] -> 0

main :: IO ()
main = do
  inputText <- readFile "input.txt"
  let input = lines inputText
  print @Int $ solve input Dir {right = 3, down = 1}
  print @Int $
    product . map (solve input) $
      [ Dir {right = 1, down = 1},
        Dir {right = 3, down = 1},
        Dir {right = 5, down = 1},
        Dir {right = 7, down = 1},
        Dir {right = 1, down = 2}
      ]