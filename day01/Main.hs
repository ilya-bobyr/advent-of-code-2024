module Main (main) where

import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, newline)
import Text.Parsec.Combinator (eof, many1, sepEndBy)
import Text.Parsec.Prim (skipMany)
import Text.Parsec.String (Parser)

inputParser :: Parser [(Int, Int)]
inputParser = do
  let lineParser = do
        leftId <- read <$> many1 digit :: Parser Int
        skipMany $ char ' '
        rightId <- read <$> many1 digit :: Parser Int
        return (leftId, rightId)

  locationPairs <- lineParser `sepEndBy` newline
  eof
  return locationPairs

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right locationPairs -> run locationPairs

run :: [(Int, Int)] -> IO ()
run locationPairs = do
  print $ findDistance locationPairs
  print $ similarityScore locationPairs

findDistance :: [(Int, Int)] -> Int
findDistance locationPairs =
  let (leftIds, rightIds) = unzip locationPairs
      leftSorted = sort leftIds
      rightSorted = sort rightIds
      distance l r = abs $ l - r
   in sum $ zipWith distance leftSorted rightSorted

similarityScore :: [(Int, Int)] -> Int
similarityScore locationPairs =
  let (leftIds, rightIds) = unzip locationPairs
      rightCounts = IntMap.fromListWith (+) $ map (\v -> (v, 1)) rightIds
   in sum $ map (\v -> v * (fromMaybe 0 $ IntMap.lookup v rightCounts)) leftIds
