module Main (main) where

import Data.Bitraversable (bitraverse)
import Data.List (elemIndex)
import Inputs (linesFor, logParse)
import Text.Read (readMaybe)

type ElfAssignments = ((Int, Int), (Int, Int))

parse :: String -> Maybe ElfAssignments
parse = (bitraverse splitInts splitInts .) . flip splitAt =<* elemIndex ','
 where
  splitInts = (bitraverse maybeInt maybeInt .) . flip splitAt =<* elemIndex '-'
  maybeInt = readMaybe . filter (`elem` ['0' .. '9'])
  infixr 1 =<*
  (=<*) f g = (=<<) . f <*> g

partOne :: [ElfAssignments] -> Int
partOne = length . filter ((||) <$> overlap' <*> overlap)
 where
  overlap' (l, r) = overlap (r, l)
  overlap ((lLow, lHigh), (rLow, rHigh)) =
    lLow <= rLow && rHigh <= lHigh

partTwo :: [ElfAssignments] -> Int
partTwo = length . filter (not . disjoint)
 where
  disjoint ((lLow, lHigh), (rLow, rHigh)) =
    lHigh < rLow || rHigh < lLow

main :: IO ()
main = do
  input <- logParse parse =<< linesFor "04"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
