module Main (main) where

import Inputs (Parser, inputFor, parsecParse)

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Map.Strict as M
import Data.Range (Range (SingletonRange), difference, fromRanges, (+=+))
import Text.Megaparsec (MonadParsec (eof), sepEndBy)
import Text.Megaparsec.Char (hspace, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Point = (Integer, Integer)
type Report = (Point, Point)
type Coverage = M.Map Point Integer

pInput :: Parser [Report]
pInput = pReport `sepEndBy` newline <* eof
 where
  pReport = do
    void $ string "Sensor at x="
    xS <- signed
    void $ string ", y="
    yS <- signed
    void $ string ": closest beacon is at x="
    xB <- signed
    void $ string ", y="
    yB <- signed
    return ((xS, yS), (xB, yB))
  signed = L.signed hspace L.decimal

mDist :: Point -> Point -> Integer
mDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

sensorCoverage :: Report -> Coverage
sensorCoverage (s, b) = M.singleton s $ mDist s b

sensorRange :: Integer -> Point -> Integer -> [Range Integer]
sensorRange r (x, y) c =
  let width = c - abs (y - r)
   in ([(x - width) +=+ (x + width) | width >= 0])

lineCoverage :: Integer -> Coverage -> [Range Integer]
lineCoverage y = M.foldMapWithKey (sensorRange y)

partOne :: [(Point, Point)] -> Int
partOne reports =
  let coverage = foldMap sensorCoverage reports
      nonEmpty = [SingletonRange x | (x, y) <- uncurry (++) $ unzip reports, y == row]
      row = 2000000
   in length $ fromRanges (lineCoverage row coverage `difference` nonEmpty)

partTwo :: [(Point, Point)] -> Integer
partTwo reports =
  let (lBound, uBound) = (0, 4000000)
      coverage = foldMap sensorCoverage reports
      differences = [([lBound +=+ uBound] `difference` lineCoverage y coverage, y) | y <- [lBound .. uBound]]
   in uncurry (+) $ first (* 4000000) $ head $ concatMap (\(range, y) -> zip (fromRanges range) (repeat y)) differences

main :: IO ()
main = do
  input <- parsecParse pInput =<< inputFor "15"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
