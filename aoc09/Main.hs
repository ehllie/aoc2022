module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set, empty, insert, size)
import Inputs (linesFor, logParse)
import Text.Read (readMaybe)

data Move = L | R | U | D
  deriving (Show, Read)

type Position = (Int, Int)

type Visited = Set Position

parse :: String -> Maybe [Move]
parse s = case words s of
  [dir, num] -> replicate <$> readMaybe num <*> readMaybe dir
  _ -> Nothing

updatePositions :: (Visited, NonEmpty Position) -> Move -> (Visited, NonEmpty Position)
updatePositions (visited, hPos :| knots) move = (last knots' `insert` visited, hPos' :| knots')
 where
  hPos' = case move of
    L -> (fst hPos - 1, snd hPos)
    R -> (fst hPos + 1, snd hPos)
    U -> (fst hPos, snd hPos + 1)
    D -> (fst hPos, snd hPos - 1)
  moveKnot h@(hX, hY) k@(kX, kY) = if adjacent h k then k else (kX + signum (hX - kX), kY + signum (hY - kY))
  adjacent (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1
  knots' = tail $ scanl moveKnot hPos' knots

partOne :: [Move] -> Int
partOne = size . fst . foldl updatePositions (empty, (0, 0) :| replicate 1 (0, 0))

partTwo :: [Move] -> Int
partTwo = size . fst . foldl updatePositions (empty, (0, 0) :| replicate 9 (0, 0))

main :: IO ()
main = do
  input <- concat <$> (logParse parse =<< linesFor "09")
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
