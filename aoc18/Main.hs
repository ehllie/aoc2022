module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Inputs (Parser, inputFor, parsecParse)
import Text.Megaparsec (MonadParsec (eof), sepEndBy)
import Text.Megaparsec.Char (char, space, space1)
import qualified Text.Megaparsec.Char.Lexer as L

data Cube = Lava | Air | Void
  deriving (Eq, Show)
type Point3 = (Int, Int, Int)
type Droplets = Map Point3 Cube

pInput :: Parser [Point3]
pInput = (pPoint `sepEndBy` space1) <* eof
 where
  pPoint =
    (,,)
      <$> signedInt
      <*> (char ',' *> signedInt)
      <*> (char ',' *> signedInt)
  signedInt = L.signed space L.decimal

surface :: Droplets -> Int
surface droplets =
  sum
    [ length $
      filter
        ((== Just Air) . (`M.lookup` droplets))
        (surrounding pt)
    | pt <- M.keys $ M.filter (== Lava) droplets
    ]

surrounding :: Point3 -> [Point3]
surrounding (x, y, z) =
  [ (x', y', z')
  | x' <- [x - 1 .. x + 1]
  , y' <- [y - 1 .. y + 1]
  , z' <- [z - 1 .. z + 1]
  , abs (x' - x) + abs (y' - y) + abs (z' - z) == 1
  ]

cornerPoints :: [Point3] -> (Point3, Point3)
cornerPoints points =
  ( (minimum xs, minimum ys, minimum zs)
  , (maximum xs, maximum ys, maximum zs)
  )
 where
  (xs, ys, zs) = unzip3 points

pointSpan :: Point3 -> Point3 -> [Point3]
pointSpan (xMin, yMin, zMin) (xMax, yMax, zMax) =
  [ (x, y, z)
  | x <- [xMin - 1 .. xMax + 1]
  , y <- [yMin - 1 .. yMax + 1]
  , z <- [zMin - 1 .. zMax + 1]
  ]

partOne :: [Point3] -> Int
partOne points =
  let lava = M.fromList $ zip points (repeat Lava)
      (minPoint, maxPoint) = cornerPoints points
      air =
        M.fromList $
          zip
            (pointSpan minPoint maxPoint)
            (repeat Air)
   in surface $ M.union lava air

partTwo :: [Point3] -> Int
partTwo points =
  let lava = M.fromList $ zip points (repeat Lava)
      (minPoint@(x, y, z), maxPoint) = cornerPoints points
      void =
        M.fromList $
          zip
            (pointSpan minPoint maxPoint)
            (repeat Void)
      preAir = M.union lava void
      air filled [] = filled
      air filled potential =
        let new =
              M.fromList
                [ (pt, Air)
                | pt <- potential
                , not $ pt `M.member` filled
                , Just Void == M.lookup pt preAir
                ]
            potential' = concatMap surrounding $ M.keys new
         in air (M.union filled new) potential'
   in surface $ M.union (air M.empty [(x - 1, y - 1, z - 1)]) preAir

main :: IO ()
main = do
  input <- parsecParse pInput =<< inputFor "18"
  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)
