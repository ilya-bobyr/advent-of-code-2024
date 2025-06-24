{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Data.Word (Word64)
import Math.NumberTheory.Logarithms (integerLog10)
import Text.Parsec (parse, sepBy)
import Text.Parsec.Char (char, digit, newline)
import Text.Parsec.Combinator (eof, many1, sepEndBy)
import Text.Parsec.Prim (skipMany)
import Text.Parsec.String (Parser)

data Equation = MkEquation {testValue :: Word64, numbers :: [Word64]}
  deriving (Show)

type Equations = [Equation]

inputParser :: Parser Equations
inputParser = do
  equations <- equationParser `sepEndBy` newline
  eof
  return equations

hspaces :: Parser ()
hspaces = skipMany $ char ' '

equationParser :: Parser Equation
equationParser = do
  testValue <- read <$> many1 digit :: Parser Word64
  hspaces
  _ <- char ':'
  hspaces
  numbers <- (read <$> many1 digit) `sepBy` hspaces :: Parser [Word64]
  return $ MkEquation testValue numbers

main :: IO ()
main = do
  input <- getContents
  case parse inputParser "<input>" input of
    Left errorText -> print errorText
    Right equations -> run equations

run :: Equations -> IO ()
run equations = do
  -- print equations
  let count1 = sum $ map testValue $ filter case1CanBeValid equations
  print count1
  let count2 = sum $ map testValue $ filter case2CanBeValid equations
  print count2

-- Case 1

case1CanBeValid :: Equation -> Bool
case1CanBeValid (MkEquation testValue (first : rest)) =
  let variants !acc [] = [acc]
      variants !acc (n : ns) =
        if acc > testValue
          then []
          else variants (acc + n) ns <> variants (acc * n) ns
   in any (== testValue) $ variants first rest
case1CanBeValid (MkEquation _ []) = error "An equation with no numbers"

-- Case 2

concatNums :: Word64 -> Word64 -> Maybe Word64
concatNums a b =
  let aLen = integerLog10 $ toInteger a
      bLen = integerLog10 $ toInteger b
   in if aLen + bLen > 19
        then Nothing
        else Just $ a * 10 ^ (bLen + 1) + b

case2CanBeValid :: Equation -> Bool
case2CanBeValid (MkEquation testValue (first : rest)) =
  let variants [] !acc = [acc]
      variants (n : ns) !acc =
        if acc > testValue
          then []
          else
            variants ns (acc + n)
              <> variants ns (acc * n)
              <> maybe [] (variants ns) (concatNums acc n)
   in any (== testValue) $ variants rest first
case2CanBeValid (MkEquation _ []) = error "An equation with no numbers"
