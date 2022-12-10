{-# LANGUAGE LambdaCase #-}

module Main where

import Inputs (linesFor, logParse)

import Data.List.Split (chunksOf)
import Text.Read (readMaybe)

data OP = Noop | AddX Int
  deriving (Show)

type Register = Int

parseLine :: String -> Maybe OP
parseLine s = case words s of
  ["noop"] -> Just Noop
  ["addx", x] -> AddX <$> readMaybe x
  _ -> Nothing

runCpu :: Register -> OP -> [Register]
runCpu r = \case
  Noop -> [r]
  AddX x -> [r, r + x]

partOne :: [OP] -> Int
partOne ops = sum [r * i | (r, i) <- zip cycles [1 ..], i `elem` [20, 60 .. 220]]
 where
  cycles = concat $ scanl (runCpu . last) [1] ops

partTwo :: [OP] -> String
partTwo ops = unlines pixels
 where
  pixels = [[if abs (r - c) <= 1 then 'X' else ' ' | (r, c) <- zip row [0 ..]] | row <- rows]
  rows = chunksOf 40 cycles
  cycles = concat $ scanl (runCpu . last) [1] ops

main :: IO ()
main = do
  input <- logParse parseLine =<< linesFor "10"
  putStrLn $ "Part 1: " ++ show (partOne input)
  putStrLn $ "Part 2: \n" ++ partTwo input
