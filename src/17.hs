{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

import Data.Foldable (Foldable(foldl'))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Data.List.Index (imapM, imap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict (execState, put, get)

main = do
  input <- readFile "17.txt" <&> lines <&> parseInput
  return ()

parseInput :: [[Char]] -> Map (Int, Int, Int) Bool
parseInput xss = execState state M.empty
  where
    state = xss
      & imapM (\i xs -> xs
        & imapM (\j x -> do
          case x of
            '#' -> do
              world <- get
              put $ M.insert (i,j,0) True world
            '.' -> return ()
            _ -> error "invalid input"
        )
      )
