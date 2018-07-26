module Main where

import EgRegex
    (  languageFromGrammar
    , testF
    )
import System.Environment (getArgs)
import System.IO
    ( hFlush
    , stdout
    )

putStrLnAndFlush s = do
    putStrLn s
    hFlush stdout

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn (languageFromGrammar (testF (head args))) --  "he(llo)*|wor+ld\\w?"
