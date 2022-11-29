module TH (compileEnv) where

import Control.Applicative (Alternative ((<|>)))
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift (lift))
import System.Environment (getEnv)

compileEnv :: String -> IO String -> Q Exp
compileEnv var fallback = lift =<< runIO (getEnv var <|> fallback)
