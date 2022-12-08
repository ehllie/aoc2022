module Main (main) where

import Data.List (findIndex, inits, tails, transpose)
import Inputs (linesFor)

parse :: String -> [Int]
parse = map (subtract (fromEnum '0') . fromEnum)

visibleOut :: [Int] -> [Bool]
visibleOut row =
  let fromLeft = init $ scanl max (-1) row
      fromRight = tail $ scanr max (-1) row
      visible tree fromL fromR = tree > fromL || tree > fromR
   in zipWith3 visible row fromLeft fromRight

visibleFrom :: [Int] -> [(Int, Int)]
visibleFrom row =
  let toLeft = init $ inits row
      toRight = tail $ tails row
      los tree toL toR =
        ( maybe (length toL) (+ 1) $ findIndex (>= tree) (reverse toL)
        , maybe (length toR) (+ 1) $ findIndex (>= tree) toR
        )
   in zipWith3 los row toLeft toRight

partOne :: [[Int]] -> Int
partOne forest =
  let rowWise = map visibleOut forest
      colWise = transpose $ map visibleOut $ transpose forest
      tallEnough = [zipWith (||) row col | (row, col) <- zip rowWise colWise]
   in length $ concatMap (filter id) tallEnough

partTwo :: [[Int]] -> Int
partTwo forest =
  let rowWise = map visibleFrom forest
      colWise = transpose $ map visibleFrom $ transpose forest
      scenicScores = [zipWith (\(l, r) (u, d) -> l * r * u * d) row col | (row, col) <- zip rowWise colWise]
   in maximum $ concat scenicScores

main :: IO ()
main = do
  input <- map parse <$> linesFor "08"
  putStrLn $ "Part 1: " ++ show (partOne input)
  putStrLn $ "Part 2: " ++ show (partTwo input)
