{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Monad.ST (ST, runST)
import Data.Array.IArray (assocs, foldlArray', (!))
import Data.Array.IArray qualified as IArray
import Data.Array.MArray (freeze, modifyArray, readArray)
import Data.Array.MArray qualified as MArray
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Bits ((.&.), (.|.))
import Data.Ix (inRange)
import Data.List (find, uncons)
import Data.Word (Word8)

type BoardIx = (Int, Int)

type Obstacles = UArray BoardIx Bool

type GuardPos = BoardIx

data GuardDir
  = GUp
  | GRight
  | GDown
  | GLeft
  deriving (Show, Eq)

type GuardState = (GuardPos, GuardDir)

main :: IO ()
main = do
  input <- getContents
  let (obstacles, guardPos) = parseBoard input
  -- print obstacles
  run obstacles (guardPos, GUp)

parseBoard :: String -> (Obstacles, GuardPos)
parseBoard input =
  let ls = lines input
      width = case uncons ls of
        Just (line, _) -> length line
        Nothing -> error "Input has no lines"
      height = length ls

      indices = ((0, 0), (fromIntegral height - 1, fromIntegral width - 1))

      obstacles = IArray.listArray indices $ map (== '#') $ concat ls

      guardPos = lookForGuard ls
      lookForGuard :: [String] -> (Int, Int)
      lookForGuard =
        let go _ [] = error "Guard position is missing from the map"
            go !y (l : ls') = case lookInLine l of
              Just !x -> (y, x)
              Nothing -> go (y + 1) ls'

            lookInLine :: String -> Maybe Int
            lookInLine line =
              fmap fst $ flip find (zip [0 ..] line) ((== '^') . snd)
         in go 0
   in (obstacles, guardPos)

run :: Obstacles -> GuardState -> IO ()
run obstacles guardStartState = do
  let (looped, visited) = runGuardWithHistory obstacles guardStartState Nothing
  if looped
    then error "Guard is looping in the initial configuration"
    else return ()

  let count1 = countVisited $ visited
  print count1

  let count2 = countLoops obstacles visited guardStartState
  print count2

-- Implementation

type BoardVisited = UArray BoardIx Word8

type STBoardVisited s = STUArray s BoardIx Word8

newtype VisitedCell = MkVisitedCell {unVisitedCell :: Word8}
  deriving (Show)

dirToVisitedCellMask :: GuardDir -> VisitedCell
dirToVisitedCellMask GUp = MkVisitedCell 1
dirToVisitedCellMask GRight = MkVisitedCell 2
dirToVisitedCellMask GDown = MkVisitedCell 4
dirToVisitedCellMask GLeft = MkVisitedCell 8

visitedContains :: GuardDir -> VisitedCell -> Bool
visitedContains dir cell =
  unVisitedCell cell .&. (unVisitedCell $ dirToVisitedCellMask dir) /= 0

markVisited :: forall s. STBoardVisited s -> GuardState -> ST s ()
markVisited visited (pos, dir) =
  let mask = unVisitedCell $ dirToVisitedCellMask dir
   in modifyArray visited pos $ (.|. mask)

isLoop :: forall s. STBoardVisited s -> GuardState -> ST s Bool
isLoop visited (pos, dir) =
  visitedContains dir <$> MkVisitedCell <$> readArray visited pos

data MoveResult
  = NewState GuardState
  | MovedOutside
  deriving (Show, Eq)

turnOnObstacle :: GuardDir -> GuardDir
turnOnObstacle GUp = GRight
turnOnObstacle GRight = GDown
turnOnObstacle GDown = GLeft
turnOnObstacle GLeft = GUp

unobstructedMove :: BoardIx -> GuardDir -> BoardIx
unobstructedMove (y, x) GUp = (y - 1, x)
unobstructedMove (y, x) GRight = (y, x + 1)
unobstructedMove (y, x) GDown = (y + 1, x)
unobstructedMove (y, x) GLeft = (y, x - 1)

tryMove :: (Obstacles, Maybe BoardIx) -> GuardState -> MoveResult
tryMove (obstacles, extraObstacle) (startPos, startDir) =
  let bounds = IArray.bounds obstacles
      tryPos = unobstructedMove startPos startDir
   in if
        | not $ inRange bounds tryPos -> MovedOutside
        | obstacles ! tryPos
            || maybe False (== tryPos) extraObstacle ->
            NewState (startPos, turnOnObstacle startDir)
        | otherwise -> NewState (tryPos, startDir)

runGuardWithHistory ::
  Obstacles ->
  GuardState ->
  Maybe BoardIx ->
  (Bool, BoardVisited)
runGuardWithHistory obstacles guardStartState extraObstacle =
  let bounds = IArray.bounds obstacles
      runGuard :: forall s. STBoardVisited s -> GuardState -> ST s Bool
      runGuard visited guardState = do
        loop <- isLoop visited guardState
        if loop
          then return True
          else do
            markVisited visited guardState
            case tryMove (obstacles, extraObstacle) guardState of
              MovedOutside -> return False
              NewState guardState' -> runGuard visited guardState'
   in runST $ do
        visited <- MArray.newArray bounds 0
        looped <- runGuard visited guardStartState
        visited' <- freeze visited
        return (looped, visited')

-- Case 1

countVisited :: BoardVisited -> Int
countVisited = foldlArray' (\acc v -> fromEnum (v /= 0) + acc) 0

-- Case 2

countLoops :: Obstacles -> BoardVisited -> GuardState -> Int
countLoops obstacles visited guardStartState@(guardStartPos, _) =
  let potentialObstacles :: [BoardIx]
      potentialObstacles =
        map fst $
          filter (\(i, v) -> v /= 0 && i /= guardStartPos) $
            assocs visited

      loopsFor extraObstacle =
        fst $ runGuardWithHistory obstacles guardStartState (Just extraObstacle)
   in sum $ map (fromEnum . loopsFor) potentialObstacles
