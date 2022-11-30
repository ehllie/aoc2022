{-# LANGUAGE TemplateHaskell #-}

module Inputs (inputFor, linesFor, logParse) where

import TH (compileEnv)

import Data.List (partition)
import Data.Maybe (fromJust, isNothing)
import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))

inputsDir :: FilePath
inputsDir = $(compileEnv "INPUTS_DIR" ((</> "inputs") <$> getCurrentDirectory))

inputFor :: String -> IO String
inputFor prob = readFile $ inputsDir </> ("input-" ++ prob) <.> "txt"

linesFor :: String -> IO [String]
linesFor = fmap lines . inputFor

-- Parse input lines with the given parser, and log failiures.
logParse :: (String -> Maybe a) -> [String] -> IO [a]
logParse p l = do
  let (failed, parsed) = partition (isNothing . snd . fst) $ zip [(s, p s) | s <- l] [1 ..]
      format ((s, _), n) = "Failed to parse line " ++ show n ++ ": " ++ s

  if null failed
    then return $ map (fromJust . snd . fst) parsed
    else mapM_ (putStrLn . format) failed >> fail "Failed to parse input"
