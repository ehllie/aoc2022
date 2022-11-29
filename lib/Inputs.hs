{-# LANGUAGE TemplateHaskell #-}

module Inputs (inputFor, linesFor) where

import TH

import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))

inputsDir :: FilePath
inputsDir = $(compileEnv "INPUTS_DIR" ((</> "inputs") <$> getCurrentDirectory))

inputFor :: String -> IO String
inputFor prob = readFile $ inputsDir </> ("input-" ++ prob) <.> "txt"

linesFor :: String -> IO [String]
linesFor = fmap lines . inputFor
