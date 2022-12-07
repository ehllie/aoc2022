module Main (main) where

import Control.Monad (foldM)
import Inputs (linesFor, logParse)
import Text.Read (readMaybe)

data FS = FS
  { name :: String
  , parent :: Maybe FS
  , subDirs :: [FS]
  , files :: [(Integer, String)]
  }
  deriving (Show)

data Op = LS | CD String | File Integer String | Dir String
  deriving (Show)

parseLine :: String -> Maybe Op
parseLine s =
  case words s of
    ["dir", name] -> Just $ Dir name
    [_, "ls"] -> Just LS
    [_, "cd", dir] -> Just $ CD dir
    [fsize, fname] -> flip File fname <$> readMaybe fsize
    _ -> Nothing

update :: FS -> Op -> Maybe FS
update fs@FS{parent, files} op = case op of
  CD ".." -> do
    parent' <- parent
    Just parent'{subDirs = fs : subDirs parent'}
  CD dir -> Just $ FS dir (Just fs) [] []
  File fsize fname -> Just fs{files = (fsize, fname) : files}
  _ -> Just fs

parse :: [Op] -> Maybe FS
parse = fmap root . foldM update (FS "/" Nothing [] [])
 where
  root = flip maybe root <*> flip update (CD "..")

flattenDirs :: FS -> [FS]
flattenDirs = (:) <*> concatMap flattenDirs . subDirs

size :: FS -> Integer
size = (+) <$> sum . map size . subDirs <*> sum . map fst . files

partOne :: FS -> Integer
partOne = sum . filter (<= 100000) . map size . flattenDirs

partTwo :: FS -> Integer
partTwo fs =
  let sizes = map size $ flattenDirs fs
      missingSpace = 30000000 - (70000000 - maximum sizes)
      bigEnough = filter (>= missingSpace) sizes
   in minimum bigEnough

main :: IO ()
main = do
  input <- parse . tail <$> (logParse parseLine =<< linesFor "07")
  putStrLn $ "Part 1: " ++ show (partOne <$> input)
  putStrLn $ "Part 2: " ++ show (partTwo <$> input)
