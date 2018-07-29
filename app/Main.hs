module Main where

import EgRegex
    ( languageFromRegexStrings
    , complementLanguageFromRegexStrings
    , showOptimizedRegex
    )
import Args
    ( Flag(..)
    , parseOptions
    )
import System.Environment (getArgs)
import System.IO
    ( hFlush
    , stdout
    )

import Data.Maybe
    ( isJust
    , fromMaybe
    )


import System.Exit (die)

shouldPrintLanguage :: Maybe Integer -> [String] -> Maybe [String]
shouldPrintLanguage Nothing = const Nothing
shouldPrintLanguage (Just 0) = Just . id
shouldPrintLanguage (Just howMany) = Just . (take (fromInteger howMany))

showMatches :: [Flag] -> [Char] -> [Either String String] -> Maybe [String]
showMatches opts extendedAlphabet =
    (shouldPrintLanguage howMany) . (languageFromRegexStrings extendedAlphabet)
  where
    howMany = foldl
        (\b x -> case x of
            ShowMatches y -> Just (case y of { Nothing -> 0 ; Just z -> z })
            _             -> b)
        Nothing
        opts

showMismatches :: [Flag] -> [Char] -> [Either String String] -> Maybe [String]
showMismatches opts extendedAlphabet =
    (shouldPrintLanguage howMany) . (complementLanguageFromRegexStrings extendedAlphabet)
  where
    howMany = foldl
        (\b x -> case x of
            ShowMismatches y -> Just (case y of { Nothing -> 0 ; Just z -> z })
            _             -> b)
        Nothing
        opts


putStrLnAndFlush :: String -> IO ()
putStrLnAndFlush s = do
    putStrLn s
    hFlush stdout

main :: IO ()
main = do
    args <- getArgs
    parsedArgs <- either die return (parseOptions args)
    regexStrings <- return $
        [Right s | (Matches s) <- parsedArgs] ++ [Left s | (DoesntMatch s) <- parsedArgs]
    extendedAlphabet <- return []
    printFlushing <- return $ mapM_ putStrLnAndFlush
    shouldShowMatches <- return $ showMatches parsedArgs extendedAlphabet regexStrings
    shouldShowMismatches <- return $ showMismatches parsedArgs extendedAlphabet regexStrings
    -- print regexStrings
    -- print shouldShowMatches
    -- print shouldShowMismatches
    if isJust shouldShowMatches
        then printFlushing (fromMaybe [] shouldShowMatches)
    else (if isJust shouldShowMismatches
        then printFlushing (fromMaybe [] shouldShowMismatches)
    else (if Optimize `elem` parsedArgs
        then putStrLnAndFlush (showOptimizedRegex extendedAlphabet regexStrings)
    else print (show parsedArgs)))
