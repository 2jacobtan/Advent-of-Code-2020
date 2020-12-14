-- In future probably should just use Megaparsec instead of the hack job of a parse via string manipulation.

-- Also should break the program down into smaller parts, instead of doing too many things in one function.


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Data.Foldable (Foldable(foldl'))
import Data.Bits ( Bits(zeroBits, (.&.), (.|.), setBit, clearBit) )
import Data.Word (Word64)
import Control.Exception (assert)
import Data.Map(Map)
import qualified Data.Map as Map
-- import qualified Debug.Trace as Debug
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main :: IO ()
main = do
  input <- readFile "14.txt" <&> lines <&> map (parseLine . words)
  print $ part1 input
  
  putStrLn "\n__Part 2"
  input2 <- readFile "14.txt" <&> lines <&> map (parseLine2 . words)
  print $ part2 input2


data Instruction = IMask Mask | IMem Mem
data Mask = Mask {m1 :: !Word64, m0 :: !Word64}

showBin :: Word64 -> ShowS
showBin = showIntAtBase 2 intToDigit
instance Show Mask where
  showsPrec _ Mask{..} = ("Mask\n" ++) . showBin m1 . ("\n"++) . showBin m0 . ("\n"++)

data Mem = Mem {loc :: !Word64, val :: !Word64} deriving Show

parseLine :: [String] -> Instruction
parseLine [left, _, right] =
  case left -- & (\x -> Debug.trace (show x) x)
  of
    "mask" ->
      let
        f m@Mask{..} (i,x) = case readMaybe @Int [x] of
          Just 1 -> m{m1 = setBit m1 i}
          Just 0 -> m{m0 = clearBit m0 i}
          Nothing -> m
          _ -> error "invalid mask"
      in
        IMask $ foldl' f Mask{m1 = zeroBits, m0 = maxBound} (right & reverse & zip [0..])
    _ ->
      let (mem,
            tail -> init -> read @Word64 -> loc) = splitAt 3 left
      in assert (mem == "mem") $
        IMem $ Mem {loc = loc, val = read @Word64 right}
parseLine xs = error $ "invalid line length: " ++ show (length xs)

data ProgState = PS {mask :: Mask, memory :: Map Word64 Word64}

runProg :: Foldable t => t Instruction -> ProgState
runProg = foldl' f (PS (Mask zeroBits zeroBits) Map.empty)
  where
    f ps@PS{mask=Mask{..},..} = \case
      IMask ma -> ps{mask = ma} -- & Debug.trace (show ma)
      IMem Mem{..} ->
        let result = val .&. m0 .|. m1 -- .&. infixl7, .|. infixl5
        in ps{memory = Map.insert loc result memory }

part1 :: Foldable t => t Instruction -> Word64
part1 (runProg -> PS{..}) = sum memory


-- Part 2

data Instruction' = IMask' Mask' | IMem' Mem
data Mask' = Mask' {m1' :: !Word64, mx :: [Int]}

parseLine2 :: [String] -> Instruction'
parseLine2 [left, _, right] =
  case left -- & (\x -> Debug.trace (show x) x)
  of
    "mask" ->
      let
        f m@Mask'{..} (i,x) = case x of
          '1' -> m{m1' = setBit m1' i}
          'X' -> m{mx = i:mx} -- collect indices of 'x' mask
          '0' -> m
          invalidMask -> error $ "invalid mask: " ++ show invalidMask
      in
        IMask' $ foldl' f Mask'{m1' = zeroBits, mx = []} (right & reverse & zip [0..])
    _ ->
      let (mem,
            tail -> init -> read @Word64 -> loc) = splitAt 3 left
      in assert (mem == "mem") $
        IMem' $ Mem {loc = loc, val = read @Word64 right}
parseLine2 xs = error $ "invalid line length: " ++ show (length xs)

data ProgState' = PS' {mask' :: Mask', memory' :: Map Word64 Word64}

runProg2 :: Foldable t => t Instruction' -> ProgState'
runProg2 = foldl' f (PS' (Mask' zeroBits []) Map.empty)
  where
    f ps@PS'{mask'=Mask'{..},..} = \case
      IMask' ma -> ps{mask' = ma} -- & Debug.trace (show ma)
      IMem' Mem{..} ->
        let locM1 = loc .|. m1'
            locM1Mx = foldl' g [locM1] mx
              where g ls i = [ h l i | h <- [setBit, clearBit], l <- ls]
            locM1MxVal = Map.fromList [(k,val) | k <- locM1Mx]
        in ps{memory' = Map.union locM1MxVal memory' }

part2 :: Foldable t => t Instruction' -> Word64
part2 (runProg2 -> PS'{..}) = sum memory'
