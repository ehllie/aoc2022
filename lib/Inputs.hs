{-# LANGUAGE TemplateHaskell #-}

module Inputs (Parser, inputFor, linesFor, logParse, parsecParse) where

import TH (compileEnv)

import Data.List (partition)
import Data.Maybe (fromJust, isNothing)
import Data.Void (Void)
import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)

type Parser = Parsec Void String

inputsDir :: FilePath
inputsDir = $(compileEnv "INPUTS_DIR" ((</> "inputs") <$> getCurrentDirectory))

inputFor :: String -> IO String
inputFor prob = readFile $ inputsDir </> ("input-" ++ prob) <.> "txt"

linesFor :: String -> IO [String]
linesFor = fmap lines . inputFor

parsecParse :: Parser a -> String -> IO a
parsecParse p input =
  case runParser p "" input of
    Left err -> putStrLn (errorBundlePretty err) >> fail "Could not parse data"
    Right r -> return r

-- Parse input lines with the given parser, and log failiures.
logParse :: (String -> Maybe a) -> [String] -> IO [a]
logParse p l = do
  let (failed, parsed) = partition (isNothing . snd . fst) $ zip [(s, p s) | s <- l] [1 ..]
      format ((s, _), n) = "Failed to parse line " ++ show n ++ ": " ++ s

  if null failed
    then return $ map (fromJust . snd . fst) parsed
    else mapM_ (putStrLn . format) failed >> fail "Failed to parse input"
