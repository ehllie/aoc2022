module Main (main) where

import Control.Monad (foldM)
import Data.List (sort)
import Inputs (inputFor)

uniqueChunk :: [Char] -> (Int, Char) -> Either Int [Char]
uniqueChunk previous (index, current) =
  let chunk = current : previous
      sorted = sort chunk
      allUnique = all (uncurry (>)) $ zip (tail sorted) sorted
   in if allUnique then Left index else Right $ init chunk

partOne :: String -> Either Int [Char]
partOne stream = foldM uniqueChunk (reverse seed) $ zip [4 ..] rest
 where
  (seed, rest) = splitAt 3 stream

partTwo :: String -> Either Int [Char]
partTwo stream = foldM uniqueChunk (reverse seed) $ zip [14 ..] rest
 where
  (seed, rest) = splitAt 13 stream

main :: IO ()
main = do
  input <- inputFor "06"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
