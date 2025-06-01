module Main (main) where

import Data.Array.IArray ((!))
import Data.Array.IArray qualified as IArray
import Data.Array.Unboxed (UArray)
import Data.Ix (inRange)
import Data.List (uncons)

type BoardIx = (Int, Int)

type Board = UArray BoardIx Char

main :: IO ()
main = do
  input <- getContents
  let board = parseBoard input
  -- print board
  run board

parseBoard :: String -> Board
parseBoard input =
  let ls = lines input
      width = case uncons ls of
        Just (line, _) -> length line
        Nothing -> error "Input has no lines"
      height = length ls

      indices = ((0, 0), (fromIntegral height - 1, fromIntegral width - 1))
   in IArray.listArray indices $ concat ls

run :: Board -> IO ()
run board = do
  let allIxs = IArray.indices board
      count1 = sum $ map (countXmas1StartingAt board) allIxs
      count2 = sum $ map (countXmas2StartingAt board) allIxs
  print count1
  print count2

-- Case 1

isXmas1At :: Board -> BoardIx -> (BoardIx -> BoardIx) -> Bool
isXmas1At board startIx shift =
  let bounds = IArray.bounds board
      mIx = shift startIx
      aIx = shift mIx
      sIx = shift aIx
      allInRange = and $ map (inRange bounds) [mIx, aIx, sIx]
   in allInRange
        && board ! startIx == 'X'
        && board ! mIx == 'M'
        && board ! aIx == 'A'
        && board ! sIx == 'S'

countXmas1StartingAt :: Board -> BoardIx -> Int
countXmas1StartingAt board startIx =
  let checkDir shift = fromEnum $ isXmas1At board startIx shift
   in sum $
        [ checkDir (\(y, x) -> (y - 1, x - 1)),
          checkDir (\(y, x) -> (y - 1, x)),
          checkDir (\(y, x) -> (y - 1, x + 1)),
          checkDir (\(y, x) -> (y, x - 1)),
          checkDir (\(y, x) -> (y, x + 1)),
          checkDir (\(y, x) -> (y + 1, x - 1)),
          checkDir (\(y, x) -> (y + 1, x)),
          checkDir (\(y, x) -> (y + 1, x + 1))
        ]

-- Case 2

countXmas2StartingAt :: Board -> BoardIx -> Int
countXmas2StartingAt board startIx =
  let (_, (height, width)) = IArray.bounds board
      goodStartingPoint =
        fst startIx > 0
          && fst startIx < height
          && snd startIx > 0
          && snd startIx < width
      (startY, startX) = startIx
      pair1 =
        [ board ! (startY - 1, startX - 1),
          board ! (startY + 1, startX + 1)
        ]
      pair2 =
        [ board ! (startY - 1, startX + 1),
          board ! (startY + 1, startX - 1)
        ]
   in fromEnum $
        goodStartingPoint
          && board ! startIx == 'A'
          && (pair1 == "MS" || pair1 == "SM")
          && (pair2 == "MS" || pair2 == "SM")
