{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Inputs (linesFor, logParse)

import Data.List.Split (chunksOf)
import Text.Read (readMaybe)

data OP = Noop | AddX Int

parseLine :: String -> Maybe OP
parseLine s = case words s of
  ["noop"] -> Just Noop
  ["addx", x] -> AddX <$> readMaybe x
  _ -> Nothing

runCpu :: [OP] -> [Int]
runCpu ops = concat $ scanl (runOp . last) [1] ops
 where
  runOp r = \case
    Noop -> [r]
    AddX x -> [r, r + x]

partOne :: [OP] -> Int
partOne ops =
  sum
    [ reg * i
    | (reg, i) <- zip (runCpu ops) [1 ..]
    , i `elem` [20, 60 .. 220]
    ]

partTwo :: [OP] -> String
partTwo ops =
  unlines
    [ [ if abs (reg - col) <= 1
        then 'X'
        else ' '
      | (reg, col) <- zip row [0 ..]
      ]
    | row <- chunksOf 40 $ runCpu ops
    ]

main :: IO ()
main = do
  input <- logParse parseLine =<< linesFor "10"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: \n" ++ partTwo input
