module Main (main) where

import Inputs (Parser, inputFor, parsecParse)

import Control.Monad (void)
import Data.List (tails)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Megaparsec (MonadParsec (eof, try), sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (letterChar, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Valve = (Int, [String])
type Graph = M.Map String Valve
type Routes = M.Map (S.Set String) Int
type Locations = [((String, S.Set String), Int)]

pInput :: Parser Graph
pInput = M.fromList <$> pValve `sepEndBy` newline <* eof
 where
  pValve = do
    void $ string "Valve "
    name <- some letterChar
    void $ string " has flow rate="
    flow <- L.decimal
    void (try $ string "; tunnels lead to valves " <|> string "; tunnel leads to valve ")
    valves <- some letterChar `sepBy` string ", "
    return (name, (flow, valves))

routesFrom :: Graph -> Locations -> Int -> Routes
routesFrom _ locations 0 = M.fromListWith max [(open, flow) | ((_, open), flow) <- locations]
routesFrom graph locations time =
  let visitValve ((current, open), flow) =
        [((next, open), flow) | next <- snd $ graph M.! current]
          ++ [ ((current, current `S.insert` open), flow + (time - 1) * localFlow)
             | let localFlow = fst $ graph M.! current
             , current `S.notMember` open
             , localFlow > 0
             ]
   in routesFrom
        graph
        (M.toList $ M.fromListWith max (concatMap visitValve locations))
        (time - 1)

partOne :: Graph -> Int
partOne graph = maximum $ routesFrom graph [(("AA", S.empty), 0)] 30

partTwo :: Graph -> Int
partTwo graph =
  let routeFlows = M.toList $ routesFrom graph [(("AA", S.empty), 0)] 26
   in maximum
        [ flow1 + flow2
        | (open1, flow1) : elephantRoutes <- tails routeFlows
        , (open2, flow2) <- elephantRoutes
        , open1 `S.disjoint` open2
        ]

main :: IO ()
main = do
  input <- parsecParse pInput =<< inputFor "16"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
