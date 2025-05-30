module Main (main) where

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import Text.Parsec (many, parse, try)
import Text.Parsec.Char (anyChar, char, digit, string)
import Text.Parsec.Combinator (eof, many1)
import Text.Parsec.String (Parser)

type Memory = String

main :: IO ()
main = do
  input <- getContents
  run input

run :: Memory -> IO ()
run memory = do
  case parse sumOfMults "<input>" memory of
    Left errorText -> print errorText
    Right res -> print res

  case parse sumOfMultsWithControl "<input>" memory of
    Left errorText -> print errorText
    Right res -> print res

sumOfMults :: Parser Int
sumOfMults = do
  vs <-
    many $
      (Just <$> try multParser)
        <|> (anyChar *> return Nothing)
  eof
  return $ sum $ catMaybes vs

data Op
  = Mult Int
  | Control Bool
  deriving (Show)

sumOfMultsWithControl :: Parser Int
sumOfMultsWithControl = do
  vs <-
    catMaybes
      <$> ( many $
              (Just . Mult <$> try multParser)
                <|> (Just . Control <$> try doDontParser)
                <|> (anyChar *> return Nothing)
          )
  eof

  let sumIfEnabled (True, acc) (Mult v) = (True, acc + v)
      sumIfEnabled (False, acc) (Mult _) = (False, acc)
      sumIfEnabled (_, acc) (Control c) = (c, acc)

  return $ snd $ foldl' sumIfEnabled (True, 0) vs

multParser :: Parser Int
multParser = do
  _ <- string "mul("
  v1 <- read <$> many1 digit :: Parser Int
  _ <- char ','
  v2 <- read <$> many1 digit :: Parser Int
  _ <- char ')'
  return $ v1 * v2

doDontParser :: Parser Bool
doDontParser = do
  (try $ string "don't()") *> return False
    <|> (try $ string "do()") *> return True
