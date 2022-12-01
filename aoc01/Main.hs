module Main where

import Data.List (sortBy)
import Inputs (linesFor)

splitEmpty :: Foldable f => [f a] -> [[f a]]
splitEmpty [] = []
splitEmpty xs =
  let (ys, zs) = break null xs
   in ys : splitEmpty (drop 1 zs)

parse :: [String] -> [[Int]]
parse = map (map read) . splitEmpty

partOne :: [[Int]] -> Int
partOne = maximum . map sum

partTwo :: [[Int]] -> Int
partTwo = sum . take 3 . sortBy (flip compare) . map sum

main :: IO ()
main = do
  input <- parse <$> linesFor "01"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
