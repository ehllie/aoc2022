module Main where

import Data.Bifunctor (bimap)
import Data.List (elemIndex)
import Inputs (linesFor, logParse)
import Text.Read (readMaybe)

type ElfAssignments = ((Int, Int), (Int, Int))

parse :: String -> Maybe ElfAssignments
parse s = do
  (l, r) <- flip splitAt s <$> elemIndex ',' s
  let readInts =
        both
          . bimap
            (readMaybe . filter (`elem` ['0' .. '9']))
            (readMaybe . filter (`elem` ['0' .. '9']))
      both (Just a, Just b) = Just (a, b)
      both _ = Nothing
  lInt <- readInts . flip splitAt l =<< elemIndex '-' l
  rInt <- readInts . flip splitAt r =<< elemIndex '-' r
  return (lInt, rInt)

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
  putStrLn $ "Part 1: " ++ show (partOne input)
  putStrLn $ "Part 2: " ++ show (partTwo input)
