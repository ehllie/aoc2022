module Main (main) where

import Control.Monad (forM_, when)
import Control.Monad.State (evalState, gets, modify)
import Data.Bitraversable (bisequence)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Inputs (linesFor)

type Point = (Int, Int)
type Node = S.Set Point
type Graph = M.Map Point Node

data DjikstraState = DS {distances :: M.Map Point Int, queue :: S.Set Point}

enumerate2D :: [[a]] -> [[(a, Int, Int)]]
enumerate2D arr =
  [ [ (i, rowN, colN)
    | (i, colN) <- zip row [0 ..]
    ]
  | (row, rowN) <- zip arr [0 ..]
  ]

parse :: String -> [Int]
parse = map parseHeight
 where
  parseHeight 'S' = parseHeight 'a'
  parseHeight 'E' = parseHeight 'z'
  parseHeight c = fromEnum c - fromEnum 'a'

findSE :: [String] -> Maybe (Point, Point)
findSE = bisequence . foldl go (Nothing, Nothing) . concat . enumerate2D
 where
  go (_, mE) ('S', row, col) = (Just (row, col), mE)
  go (mS, _) ('E', row, col) = (mS, Just (row, col))
  go acc _ = acc

buildGraph :: [[Int]] -> Graph
buildGraph heights = M.map snd . foldl addNode lonelyNodes . concat . enumerate2D $ heights
 where
  updateNode (h, old) (h', new) = (max h h', S.union old new)
  points = concat $ enumerate2D heights
  lonelyNodes = M.fromAscList [((r, c), (h, S.empty)) | (h, r, c) <- points]
  addNode graph (h, r, c) =
    let neighbours =
          M.keys $
            M.filterWithKey
              ( \p n ->
                  ( p
                      `elem` [ (r - i, c - j)
                             | i <- [-1, 0, 1]
                             , j <- [-1, 0, 1]
                             , abs i + abs j == 1
                             ]
                  )
                    && fst n - h <= 1
              )
              graph
     in M.insertWith updateNode (r, c) (h, S.fromList neighbours) graph

dijkstra :: M.Map Point (S.Set Point) -> Point -> M.Map Point Int
dijkstra graph start =
  let initState =
        DS
          { distances = M.singleton start 0
          , queue = S.singleton start
          }
      updateDist (Just d) d' = (d' < d, d')
      updateDist Nothing d' = (True, d')
      runDjikstra = do
        q <- gets queue
        if S.null q
          then gets distances
          else do
            let point = S.findMin q
                node = graph M.! point
            dist <- gets $ (M.! point) . distances
            forM_ (S.toList node) $ \point' -> do
              (shorter, dist') <- gets (flip updateDist (dist + 1) . M.lookup point' . distances)
              when shorter $ modify $ \s@DS{queue, distances} ->
                s
                  { queue = S.insert point' queue
                  , distances = M.insert point' dist' distances
                  }
            modify $ \s@DS{queue} -> s{queue = S.delete point queue}
            runDjikstra
   in evalState runDjikstra initState

partOne :: [[Int]] -> Point -> Point -> Int
partOne heights start end = distMap M.! end
 where
  distMap = dijkstra (buildGraph heights) start

partTwo :: [[Int]] -> Point -> Int
partTwo heights end = M.foldl min maxBound $ M.restrictKeys distMap lowest
 where
  distMap = dijkstra graph end
  graph = buildGraph inverted
  inverted = map (map (`subtract` fromEnum 'z')) heights
  lowest = foldl (\acc (h, r, c) -> if h == 0 then S.insert (r, c) acc else acc) S.empty . concat $ enumerate2D heights

main :: IO ()
main = do
  input <- linesFor "12"
  let heights = map parse input
      route = findSE input
  putStrLn $ "Part One: " ++ show (uncurry (partOne heights) <$> route)
  putStrLn $ "Part Two: " ++ show (partTwo heights . snd <$> route)
