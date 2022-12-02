module Main where

import Data.Bifunctor (bimap)
import Inputs (linesFor, logParse)
import Text.Read (readMaybe)

data Move = Rock | Paper | Scissors
  deriving (Eq, Show, Enum)

data ElfMove = A | B | C
  deriving (Eq, Show, Enum, Read)

data MyMove = X | Y | Z
  deriving (Eq, Show, Enum, Read)

instance Ord Move where
  (<=) :: Move -> Move -> Bool
  Rock     <= Paper    = True
  Paper    <= Scissors = True
  Scissors <= Rock     = True
  _        <= _        = False

parse :: String -> Maybe (ElfMove, MyMove)
parse s = do
  (elf, me) <- case words s of
    [elf, me] -> Just (elf, me)
    _ -> Nothing
  eMove <- readMaybe elf
  mMove <- readMaybe me
  Just (eMove, mMove)

roundScore :: (Move, Move) -> Int
roundScore (eMove, mMove) =
  1 + fromEnum mMove
    + case eMove `compare` mMove of
      LT -> 6
      EQ -> 3
      GT -> 0

partOne :: [(ElfMove, MyMove)] -> Int
partOne = sum . map (roundScore . bimap (toEnum . fromEnum) (toEnum . fromEnum))

partTwo :: [(ElfMove, MyMove)] -> Int
partTwo = sum . map
  ( roundScore . uncurry
    \m -> (toEnum $ fromEnum m,)
        . toEnum
        . (`mod` 3)
        . (+) (2 + fromEnum m)
        . fromEnum
  )

main :: IO ()
main = do
  input <- logParse parse =<< linesFor "02"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
