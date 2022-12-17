module Main (main) where

import Inputs (Parser, inputFor, parsecParse)

import Control.Monad.State (State, execState, gets, modify, replicateM_)
import Data.Bifunctor (Bifunctor (bimap, first, second))
import qualified Data.Set as S
import Text.Megaparsec (MonadParsec (eof), many, (<|>))
import Text.Megaparsec.Char (char, newline)

data Move = L | R deriving (Eq)
type Point = (Integer, Integer)
type Rocks = S.Set Point
data GameState = GS {board :: Rocks, shapeQ :: [Rocks], moveQ :: [Move], lowest :: Integer}

chamberWidth :: Integer
chamberWidth = 7

shapes :: [Rocks]
shapes =
  map
    S.fromList
    [ [(0, 0), (1, 0), (2, 0), (3, 0)]
    , [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
    , [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
    , [(0, 0), (0, 1), (0, 2), (0, 3)]
    , [(0, 0), (0, 1), (1, 0), (1, 1)]
    ]

nextShape :: State GameState Rocks
nextShape = do
  queue <- gets shapeQ
  highestPoint <- gets $ S.foldl (\acc -> max acc . snd) 0 . board
  modify $ \gs -> gs{shapeQ = tail queue}
  return $ S.map (bimap (+ 2) (+ (highestPoint + 4))) (head queue)

nextMove :: State GameState Move
nextMove = do
  moves <- gets moveQ
  modify $ \gs -> gs{moveQ = tail moves}
  return $ head moves

moveShape :: Rocks -> State GameState Rocks
moveShape shape = do
  move <- nextMove
  board <- gets board
  let shape' = S.map (first (if move == L then pred else succ)) shape
  if S.disjoint board shape'
    && all (((&&) <$> (>= 0) <*> (< chamberWidth)) . fst) shape'
    then return shape'
    else return shape

dropShape :: Rocks -> State GameState ()
dropShape shape = do
  board <- gets board
  moved <- moveShape shape
  let shape' = S.map (second pred) moved
  if S.disjoint board shape'
    then dropShape shape'
    else modify $ \gs -> gs{board = S.union board moved}

surfaceAir :: Rocks -> Rocks
surfaceAir board =
  let highestPoint = S.foldl (\acc -> max acc . snd) 0 board
      columns =
        [ takeWhile
          (`S.notMember` board)
          ([(x, y) | y <- [highestPoint + 1, highestPoint ..]])
        | x <- [0 .. chamberWidth - 1]
        ]
      expandAir air =
        let air' =
              S.foldl
                ( \acc a@(x, y) ->
                    S.union acc $
                      S.fromList
                        ( a
                            : [ a'
                              | x' <- [x - 1, x + 1]
                              , let a' = (x', y - 1)
                              , a' `S.notMember` board
                              , (x, y - 1) `S.notMember` board
                              , x' < chamberWidth
                              , x' >= 0
                              ]
                            ++ [(x, y - 1) | (x, y - 1) `S.notMember` board]
                        )
                )
                S.empty
                air
         in if S.size air' > S.size air
              then expandAir air'
              else air'
   in expandAir $ foldMap S.fromList columns

pruneDead :: Rocks -> Rocks
pruneDead board =
  let air = surfaceAir board
   in S.filter
        ( \(x, y) ->
            any (`elem` air) [(x - 1, y), (x + 1, y), (x, y + 1)]
        )
        board

scaleFloor :: State GameState ()
scaleFloor = do
  prune <- gets $ pruneDead . board
  let (_, y) = S.findMin prune
      lowestStone = S.foldl (\acc -> min acc . snd) y prune
  modify $ \gs@GS{lowest} ->
    gs
      { lowest = lowest + toInteger lowestStone
      , board = S.map (second (subtract lowestStone)) prune
      }
  return ()

pInput :: Parser [Move]
pInput = many ((char '<' >> return L) <|> (char '>' >> return R)) <* many newline <* eof

partOne :: [Move] -> Integer
partOne moves =
  let initState = GS (S.fromList [(x, 0) | x <- [0 .. 6]]) (cycle shapes) (cycle moves) 0
      GS{lowest, board} = execState (replicateM_ 2022 (nextShape >>= dropShape >> scaleFloor)) initState
   in lowest + S.foldl (\acc -> max acc . snd) 0 board

partTwo :: [Move] -> Integer
partTwo moves =
  let initState = GS (S.fromList [(x, 0) | x <- [0 .. 6]]) (cycle shapes) (cycle moves) 0
      GS{lowest, board} = execState (replicateM_ 1_000_000_000_000 (nextShape >>= dropShape >> scaleFloor)) initState
   in lowest + S.foldl (\acc -> max acc . snd) 0 board

main :: IO ()
main = do
  input <- parsecParse pInput =<< inputFor "17"
  putStrLn $ "Part One: " ++ show (partOne input)
  -- Does not work, didn't feel checking for loops
  putStrLn $ "Part Two: " ++ show (partTwo input)
