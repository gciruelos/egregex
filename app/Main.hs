module Main where

import EgRegex
    ( languageFromRegexString
    , complementLanguageFromRegexString
    )
import System.Environment (getArgs)
import System.IO
    ( hFlush
    , stdout
    )

putStrLnAndFlush :: String -> IO ()
putStrLnAndFlush s = do
    putStrLn s
    hFlush stdout

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLnAndFlush (languageFromRegexString (head args))
