{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_, replicateM)
import Control.Monad.State (
  State,
  evalState,
  gets,
  modify,
 )
import Data.List (elemIndex, sortBy)
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Map.Strict as M
import GHC.Base (join)
import Inputs (linesFor)
import Text.Read (readMaybe)

data Monkey = Monkey {items :: [Integer], op :: Integer -> Integer, test :: Integer, onTrue :: Int, onFalse :: Int}

instance Show Monkey where
  show = show . items

type MonkeyState = State (M.Map Int Monkey)

parseMonkey :: [String] -> Maybe Monkey
parseMonkey [_, itemsLine, opLine, testLine, onTrueLine, onFalseLine] = do
  let afterColon ln = flip drop ln . (+ 2) <$> elemIndex ':' ln
  items <- traverse readMaybe . splitOn ", " =<< afterColon itemsLine
  op <-
    afterColon opLine
      >>= ( \case
              [_, _, _, fnS, rhsS] -> do
                fn <- case fnS of
                  "+" -> Just (+)
                  "*" -> Just (*)
                  _ -> Nothing
                case rhsS of
                  "old" -> Just $ \x -> fn x x
                  i -> fn <$> readMaybe i
              _ -> Nothing
          )
        . words
  test <- readMaybe (last $ words testLine)
  onTrue <- readMaybe (last $ words onTrueLine)
  onFalse <- readMaybe (last $ words onFalseLine)
  Just $ Monkey{items, op, test, onTrue, onFalse}
parseMonkey _ = Nothing

playTurn :: Bool -> Int -> MonkeyState Integer
playTurn relief mId = do
  Monkey{items, op, test, onTrue, onFalse} <- gets (M.! mId)
  forM_ items $ \i -> do
    let worry = if relief then op i `div` 3 else op i
        dest = if worry `mod` test == 0 then onTrue else onFalse
    modify $ M.adjust (\m@Monkey{items} -> m{items = items ++ [worry]}) dest
  modify $ M.adjust (\m -> m{items = []}) mId
  return . toInteger $ length items

playRound :: Bool -> MonkeyState [Integer]
playRound relief = do
  lcm' <- gets $ foldl1 lcm . M.elems . M.map test
  modify $ M.map (\m@Monkey{items} -> m{items = map (`mod` lcm') items})
  join (gets (traverse (playTurn relief) . M.keys))

partOne :: [Monkey] -> Integer
partOne monkeys =
  let initState = M.fromAscList [(i, monkey) | (i, monkey) <- zip [0 ..] monkeys]
      rounds = evalState (replicateM 20 (playRound True)) initState
      activity = foldl1 (zipWith (+)) rounds
   in product . take 2 $ sortBy (flip compare) activity

partTwo :: [Monkey] -> Integer
partTwo monkeys =
  let initState = M.fromAscList [(i, monkey) | (i, monkey) <- zip [0 ..] monkeys]
      rounds = evalState (replicateM 10000 (playRound False)) initState
      activity = foldl1 (zipWith (+)) rounds
   in product . take 2 $ sortBy (flip compare) activity

main :: IO ()
main = do
  input <- traverse parseMonkey . splitWhen null <$> linesFor "11"
  putStrLn $ "Part 1: " ++ show (partOne <$> input)
  putStrLn $ "Part 2: " ++ show (partTwo <$> input)
