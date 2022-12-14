module Main (main) where

import Inputs (Parser, inputFor, parsecParse)

import Control.Arrow ((***))
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Text.Megaparsec (MonadParsec (eof), sepBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Point = (Int, Int)
type Rock = [Point]
type Tiles = M.Map Point Tile

data Tile = Air | Rock | Sand
  deriving (Eq)

pInput :: Parser [Rock]
pInput = pRock `sepBy` newline <* eof
 where
  pRock = pPoint `sepBy` string " -> "
  pPoint = (,) <$> decimal <* char ',' <*> decimal

placeTiles :: Bool -> Point -> [Rock] -> Tiles
placeTiles addFloor (sx, sy) =
  let rockTiles =
        M.fromList
          . concatMap
            ( \((xa, ya), (xb, yb)) ->
                [ ((x, y), Rock)
                | x <- [min xa xb .. max xa xb]
                , y <- [min ya yb .. max ya yb]
                ]
            )
          . (zip <*> tail)
      fillFloor tiles =
        let floorY = maximum (map snd $ M.keys tiles) + 2
            hDiff = floorY - sy
            floorPoints =
              [ ((x, floorY), Rock)
              | x <- [sx - hDiff .. sx + hDiff]
              ]
         in if addFloor
              then tiles `M.union` M.fromAscList floorPoints
              else tiles
      fillAir tiles =
        let (minX, minY) = (minimum *** minimum) $ unzip $ M.keys tiles
            (maxX, maxY) = (maximum *** maximum) $ unzip $ M.keys tiles
            airPoints =
              [ ((x, y), Air)
              | x <- [min sx minX .. max sx maxX]
              , y <- [min sy minY .. max sy maxY]
              ]
         in tiles `M.union` M.fromAscList airPoints
   in fillAir . fillFloor . foldMap rockTiles

moveGrain :: Tiles -> Point -> Maybe Point
moveGrain tiles (x, y) =
  let moveDirections = map ((,) <*> (`M.lookup` tiles)) [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
      next = find ((== Just Air) . snd) moveDirections
   in case next of
        Just (p, _) -> Just p
        Nothing -> if all (isJust . snd) moveDirections then Just (x, y) else Nothing

dropGrain :: Point -> Tiles -> Maybe Tiles
dropGrain point tiles = do
  next <- moveGrain tiles point
  if next == point
    then Just $ M.insert next Sand tiles
    else dropGrain next tiles

partOne :: [Rock] -> Int
partOne rocks =
  let tiles = placeTiles False (500, 0) rocks
      steps = iterate (dropGrain (500, 0) =<<) $ Just tiles
   in length (takeWhile isJust steps) - 1

partTwo :: [Rock] -> Int
partTwo rocks =
  let tiles = placeTiles True (500, 0) rocks
      steps = iterate (dropGrain (500, 0) =<<) $ Just tiles
   in length $ takeWhile (maybe False $ (== Air) . (M.! (500, 0))) steps

main :: IO ()
main = do
  rocks <- parsecParse pInput =<< inputFor "14"
  putStrLn $ "Part 1: " ++ show (partOne rocks)
  putStrLn $ "Part 2: " ++ show (partTwo rocks)
