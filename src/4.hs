{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
import Control.Monad (replicateM_)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Foldable (asum)

type Parser = Parsec Void Text

requiredKeys =
  Set.fromList
    [ "byr",
      "iyr",
      "eyr",
      "hgt",
      "hcl",
      "ecl",
      "pid"
      -- "cid" -- not needed
    ]

parseKeyVal :: Parser (String, String)
parseKeyVal = (,) <$> some letterChar <* char ':' <*>
  do
    pound <- optional (char '#')
    maybe id (const ('#':)) pound <$> some alphaNumChar

parseLine :: Parser [(String, String)]
parseLine = (:) <$> parseKeyVal <*> many (try $ spaceChar *> parseKeyVal)

parseText :: Parser [[(String, String)]]
parseText = sepBy parseLine (replicateM_ 2 newline)

isValidLine :: Set String -> [(String, a)] -> Bool
isValidLine reqKeys line =
  reqKeys `Set.isSubsetOf` Set.fromList (fst <$> line)

countValidLines :: (a -> Bool) -> [a] -> Int
countValidLines pred = length . filter pred

-- Part Two
isValidLine2 :: [(String, String)] -> Bool
isValidLine2 line = case runReaderT test dict of
  Just _ -> True
  Nothing -> False
  where
    dict = Map.fromList line
    test = do
      byr <- read @Int <$> ReaderT (Map.lookup "byr")
      lift $ if 1920 <= byr && byr <= 2002 then Just () else Nothing

      iyr <- read @Int <$> ReaderT (Map.lookup "iyr")
      lift $ if 2010 <= iyr && iyr <= 2020 then Just () else Nothing
      
      eyr <- read @Int <$> ReaderT (Map.lookup "eyr")
      lift $ if 2020 <= eyr && eyr <= 2030 then Just () else Nothing
      
      hgt <- ReaderT (Map.lookup "hgt")
      lift $ case reverse hgt of
        (splitAt 2 -> ("mc", read . reverse -> cm)) -> if 150 <= cm && cm <= 193 then Just () else Nothing
        (splitAt 2 -> ("ni", read . reverse -> inch)) -> if 59 <= inch && inch <= 76 then Just () else Nothing
        _ -> Nothing
      
      hcl <- ReaderT (Map.lookup "hcl")
      lift $ either (const Nothing) (const $ Just ()) $ runParser
        ((char '#' :: Parser Char) >> replicateM_ 6 (digitChar <|> asum (map char ['a'..'f'])) >> eof) "" (pack hcl)
      
      ecl <- ReaderT (Map.lookup "ecl")
      lift $ if ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] then Just () else Nothing
      
      pid <- ReaderT (Map.lookup "pid")
      lift $ either (const Nothing) (const $ Just ()) $ runParser
        ((replicateM_ 9 digitChar :: Parser ()) >> eof) "" (pack pid)

main :: IO ()
main = do
  inputText <- pack <$> readFile "4.txt"
  let inputLines =
        either (const []) id $
          runParser parseText "" inputText
      count = countValidLines (isValidLine requiredKeys) inputLines
      count2 = countValidLines isValidLine2 inputLines
  print count
  print count2

-- debug use
z :: IO Text
z = pack <$> readFile "4.txt"