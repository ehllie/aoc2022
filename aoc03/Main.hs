module Main where

import Data.Bifunctor (bimap)
import Data.List.Split (chunksOf)
import Data.Set (elemAt, fromList, intersection)
import Inputs (linesFor)

priority :: Char -> Int
priority c
  | c `elem` ['A' .. 'Z'] = 27 + fromEnum c - fromEnum 'A'
  | otherwise = 1 + fromEnum c - fromEnum 'a'

partOne :: [String] -> Int
partOne =
  sum
    . map
      ( priority
          . elemAt 0
          . uncurry intersection
          . bimap fromList fromList
          . (splitAt =<< (`div` 2) . length)
      )

partTwo :: [String] -> Int
partTwo =
  sum
    . map
      ( priority
          . elemAt 0
          . foldl1 intersection
          . map fromList
      )
    . chunksOf 3

main :: IO ()
main = do
  input <- linesFor "03"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
