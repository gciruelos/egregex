module EgRegex
    ( languageFromRegexStrings
    , showOptimizedRegex
    ) where

import EgRegexImpl

languageFromRegexStrings :: [Char] -> Bool -> [Either String String] -> [String]
languageFromRegexStrings extendedAlphabet shouldComplement =
    languageFromGrammar .
    optimizeGrammar .
    convertDFAToGrammar .
    minimizeDFA .
    simplifyPowersetConstruction .
    determinizeNFA .
    relaxOneAccepting .
    regexToNFA .
    (if shouldComplement then Complement extendedAlphabet else id) .
    (parseRegexes extendedAlphabet)

showOptimizedRegex :: [Char] -> Integer -> [Either String String] -> String
showOptimizedRegex extendedAlphabet level =
    regexToRegexString .
    (if level > 0 then optimizeRegexHarder else id) .
    optimizeRegex .
    convertDFAToRegex .
    minimizeDFA .
    simplifyPowersetConstruction .
    determinizeNFA .
    relaxOneAccepting .
    regexToNFA .
    (parseRegexes extendedAlphabet)
