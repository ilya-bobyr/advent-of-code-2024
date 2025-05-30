module Main (main) where

import Text.Parsec (parse, sepBy)
import Text.Parsec.Char (char, digit, newline)
import Text.Parsec.Combinator (eof, many1, sepEndBy)
import Text.Parsec.String (Parser)

type Report = [Int]

reportParser :: Parser Report
reportParser = do
  let levelParser = read <$> many1 digit :: Parser Int
  levelParser `sepBy` char ' '

inputParser :: Parser [Report]
inputParser = do
  reports <- reportParser `sepEndBy` newline
  eof
  return $ filter (not . null) reports

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right v -> run v

run :: [Report] -> IO ()
run reports = do
  print $ countSafe reports
  print $ countSafeWithDampener reports

-- mapM_ (putStrLn . show) $
--   filter (isSafeForEitherWithDumper safeDescPair safeAscPair) reports

type SafePairP = Int -> Int -> Bool

safeDescPair :: SafePairP
safeDescPair a b = a - 3 <= b && b <= a - 1

safeAscPair :: SafePairP
safeAscPair a b = a + 1 <= b && b <= a + 3

-- No skips

isSafeFor :: SafePairP -> Report -> Bool
isSafeFor p report = and $ zipWith p report (drop 1 report)

isSafeForEither :: SafePairP -> SafePairP -> Report -> Bool
isSafeForEither p1 p2 report = isSafeFor p1 report || isSafeFor p2 report

countSafeFor :: SafePairP -> SafePairP -> [Report] -> Int
countSafeFor p1 p2 reports = length $ filter (isSafeForEither p1 p2) reports

countSafe :: [Report] -> Int
countSafe = countSafeFor safeDescPair safeAscPair

-- With dampener

skipOne :: Int -> Report -> Report
skipOne 0 (_ : xs) = xs
skipOne i (x : xs) = x : skipOne (i - 1) xs
skipOne i [] = error $ "skipOne: index is beyond the report length: " <> show i

isSafeForEitherWithDumper :: SafePairP -> SafePairP -> Report -> Bool
isSafeForEitherWithDumper p1 p2 report =
  let variants =
        report : zipWith skipOne [0 .. (length report - 1)] (repeat report)
   in any (isSafeForEither p1 p2) variants

countSafeWithDumpenerFor :: SafePairP -> SafePairP -> [Report] -> Int
countSafeWithDumpenerFor p1 p2 reports =
  length $ filter (isSafeForEitherWithDumper p1 p2) reports

countSafeWithDampener :: [Report] -> Int
countSafeWithDampener = countSafeWithDumpenerFor safeDescPair safeAscPair
