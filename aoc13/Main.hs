module Main (main) where

import Inputs (inputFor)

import Data.List (findIndices, sort)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (eof),
  Parsec,
  errorBundlePretty,
  runParser,
  sepBy,
  (<|>),
 )
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Packet = Val Int | List [Packet]
  deriving (Eq)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Val left) (Val right) = left `compare` right
  compare (Val left) right = List [Val left] `compare` right
  compare left (Val right) = left `compare` List [Val right]
  compare (List left) (List right) = left `compare` right

pInput :: Parser [(Packet, Packet)]
pInput = (pPair `sepBy` newline) <* eof
 where
  pPair = (,) <$> pPacket <* newline <*> pPacket <* newline
  pPacket = pList <|> (Val <$> decimal)
  pList = List <$> (char '[' *> pPacket `sepBy` char ',' <* char ']')

partOne :: [(Packet, Packet)] -> Int
partOne = sum . map (+ 1) . findIndices (uncurry (<))

partTwo :: [(Packet, Packet)] -> Int
partTwo pairs =
  let separators = [List [List [Val 2]], List [List [Val 6]]]
      sorted = sort $ separators ++ concatMap (\(l, r) -> [l, r]) pairs
      indecies = findIndices (`elem` separators) sorted
   in product $ map (+ 1) indecies

main :: IO ()
main = do
  input <- inputFor "13"
  case runParser pInput "AoC-Day13" input of
    Left err -> putStr $ errorBundlePretty err
    Right pairs -> do
      putStrLn $ "Part 1: " ++ show (partOne pairs)
      putStrLn $ "Part 2: " ++ show (partTwo pairs)
