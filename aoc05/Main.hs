module Main (main) where

import Data.Char (isAlphaNum)
import Data.List (findIndex, transpose)
import Data.Maybe (fromJust)
import Inputs (linesFor)

type Crate = Char
type Stack = [Crate]
type Move = (Int, Int, Int)

emptyLine :: String -> Bool
emptyLine = not . any isAlphaNum

parseStacks :: [String] -> [Stack]
parseStacks s =
  let longest = maximum $ map length s
      padded = map (take longest . (++ repeat ' ')) s
      rotated = transpose $ reverse padded
   in [ reverse . filter (/= ' ') . drop 1 $ c
      | (c, i) <- zip (drop 1 rotated) [0 ..]
      , i `mod` 4 == 0
      ]

parseMove :: String -> Move
parseMove =
  ( (,,)
      <$> read . (!! 1)
      <*> read . (!! 3)
      <*> read . (!! 5)
  )
    . words

parse :: [String] -> Maybe ([Stack], [Move])
parse input = do
  (stacksIn, _ : movesIn) <- flip splitAt input <$> findIndex emptyLine input
  let moves = map parseMove movesIn
      stacks = parseStacks stacksIn
  return (stacks, moves)

update :: Int -> a -> [a] -> [a]
update i x xs = take i xs ++ [x] ++ drop (i + 1) xs

moveCrates :: (Stack -> Stack) -> [Stack] -> Move -> [Stack]
moveCrates stackTransform stacks (count, from, to) =
  let (picked, from') = splitAt count (stacks !! (from - 1))
      to' = stackTransform picked ++ (stacks !! (to - 1))
   in update (from - 1) from' $ update (to - 1) to' stacks

partOne :: ([Stack], [Move]) -> String
partOne (stacks, moves) = map head $ foldl (moveCrates reverse) stacks moves

partTwo :: ([Stack], [Move]) -> String
partTwo (stacks, moves) = map head $ foldl (moveCrates id) stacks moves

main :: IO ()
main = do
  input <- fromJust . parse <$> linesFor "05"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
