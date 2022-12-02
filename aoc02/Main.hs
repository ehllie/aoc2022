module Main where

import Data.Bifunctor (second)
import Inputs (linesFor, logParse)

data Move = Rock | Paper | Scissors
  deriving (Eq, Show, Enum)

data MyMove = X | Y | Z
  deriving (Eq, Show, Enum)

instance Ord Move where
  (<=) :: Move -> Move -> Bool
  Rock <= Paper = True
  Paper <= Scissors = True
  Scissors <= Rock = True
  _ <= _ = False

parse :: String -> Maybe (Move, MyMove)
parse s = do
  [elf, me] <- case words s of
    [elf, me] -> Just [elf, me]
    _ -> Nothing
  eMove <- case elf of
    "A" -> Just Rock
    "B" -> Just Paper
    "C" -> Just Scissors
    _ -> Nothing
  mMove <- case me of
    "X" -> Just X
    "Y" -> Just Y
    "Z" -> Just Z
    _ -> Nothing
  return (eMove, mMove)

interpretOne :: MyMove -> Move
interpretOne = toEnum . fromEnum

interpretTwo :: Move -> MyMove -> (Move, Move)
interpretTwo m Y = (m, m)
interpretTwo m r = (,) m case r of
  X -> moves !! (fromEnum m + 2)
  Z -> moves !! (fromEnum m + 1)
 where
  moves = cycle [Rock, Paper, Scissors]

roundScore :: (Move, Move) -> Int
roundScore (eMove, mMove) =
  1
    + fromEnum mMove
    + case eMove `compare` mMove of
      LT -> 6
      EQ -> 3
      GT -> 0

partOne :: [(Move, MyMove)] -> Int
partOne = sum . map (roundScore . second interpretOne)

partTwo :: [(Move, MyMove)] -> Int
partTwo = sum . map (roundScore . uncurry interpretTwo)

main :: IO ()
main = do
  input <- logParse parse =<< linesFor "02"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
