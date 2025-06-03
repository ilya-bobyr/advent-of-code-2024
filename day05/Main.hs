module Main (main) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (foldl', partition)
import Data.Maybe (fromMaybe)
import Text.Parsec (parse, sepBy)
import Text.Parsec.Char (char, digit, newline)
import Text.Parsec.Combinator (eof, many1, sepEndBy)
import Text.Parsec.String (Parser)

-- (before, after)
type Rule = (Int, Int)

type Update = [Int]

inputParser :: Parser ([Rule], [Update])
inputParser = do
  rules <- ruleParser `sepEndBy` newline
  _ <- newline
  updates <- updateParser `sepEndBy` newline
  eof
  return (rules, filter (not . null) updates)

ruleParser :: Parser Rule
ruleParser = do
  before <- read <$> many1 digit :: Parser Int
  _ <- char '|'
  after <- read <$> many1 digit :: Parser Int
  return (before, after)

updateParser :: Parser Update
updateParser =
  (read <$> many1 digit) `sepBy` char ','

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right (rules, updates) -> run rules updates

run :: [Rule] -> [Update] -> IO ()
run rules updates = do
  let mustNotFollow = mustNotFollowFor rules
      (correctlyOrdered, incorrectlyOrdered) =
        partitionUpdates mustNotFollow updates

  print $ updatesScore correctlyOrdered

  print $ updatesScore $ map (fixUpdate mustNotFollow) incorrectlyOrdered

updateMiddlePage :: Update -> Int
updateMiddlePage update =
  let pages = length update
   in update !! (pages `div` 2)

updatesScore :: [Update] -> Int
updatesScore = sum . map updateMiddlePage

-- Case 1

type MustNotFollow = IntMap IntSet

mustNotFollowFor :: [Rule] -> MustNotFollow
mustNotFollowFor rules =
  IntMap.fromListWith IntSet.union $
    map (\(before, after) -> (after, IntSet.singleton before)) rules

isRigthOrder :: MustNotFollow -> Update -> Bool
isRigthOrder mustNotFollow update =
  let checkUpdatePage :: (Bool, IntSet) -> Int -> (Bool, IntSet)
      checkUpdatePage (False, notAllowed) _ = (False, notAllowed)
      checkUpdatePage (True, notAllowed) page
        | IntSet.member page notAllowed = (False, notAllowed)
        | otherwise =
            let notAllowed' =
                  fromMaybe notAllowed $
                    fmap (IntSet.union notAllowed) $
                      IntMap.lookup page mustNotFollow
             in (True, notAllowed')
   in fst $ foldl' checkUpdatePage (True, IntSet.empty) update

-- Separate updates into those with correct and incorrect order based on the
-- specified ordering rules.
partitionUpdates :: MustNotFollow -> [Update] -> ([Update], [Update])
partitionUpdates mustNotFollow updates =
  partition (isRigthOrder mustNotFollow) updates

-- Case 2

fixUpdate :: MustNotFollow -> Update -> Update
fixUpdate mustNotFollow update =
  let allUpdatePages = IntSet.fromList update

      canIncludePage blocking page =
        fromMaybe True $
          fmap (IntSet.null . IntSet.intersection blocking) $
            IntMap.lookup page mustNotFollow

      (canIncludeFromStart, blockedFromStart) =
        partition (canIncludePage allUpdatePages) update

      processPage :: Update -> [Int] -> IntSet -> [Int] -> [Int]
      processPage res [] _ _ = reverse res
      processPage res (page : unblocked) blocking pending =
        let blocking' = IntSet.delete page blocking

            (newlyUnblocked, pending') =
              partition (canIncludePage blocking') pending

            res' = page : res

            unblocked' = unblocked ++ newlyUnblocked
         in processPage res' unblocked' blocking' pending'
   in processPage [] canIncludeFromStart allUpdatePages blockedFromStart
