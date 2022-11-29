module Main where

import Inputs (linesFor)

main :: IO ()
main = mapM_ putStrLn =<< linesFor "06"
