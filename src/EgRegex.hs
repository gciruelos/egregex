module EgRegex
    ( languageFromRegexStrings
    , complementLanguageFromRegexStrings
    , showOptimizedRegex
    ) where

import EgRegexImpl

languageFromRegexStrings :: [Char] -> [Either String String] -> [String]
languageFromRegexStrings extendedAlphabet =
    languageFromGrammar .
    optimizeGrammar .
    convertDFAToGrammar .
    minimizeDFA .
    simplifyPowersetConstruction .
    determinizeNFA .
    relaxOneAccepting .
    regexToNFA .
    (parseRegexes extendedAlphabet)

complementLanguageFromRegexStrings :: [Char] -> [Either String String] -> [String]
complementLanguageFromRegexStrings extendedAlphabet =
    languageFromGrammar .
    optimizeGrammar .
    convertDFAToGrammar .
    minimizeDFA .
    simplifyPowersetConstruction .
    determinizeNFA .
    relaxOneAccepting .
    regexToNFA .
    (Complement extendedAlphabet) .
    (parseRegexes extendedAlphabet)

showOptimizedRegex :: [Char] -> [Either String String] -> String
showOptimizedRegex extendedAlphabet =
    regexToRegexString .
    convertDFAToRegex .
    minimizeDFA .
    simplifyPowersetConstruction .
    determinizeNFA .
    relaxOneAccepting .
    regexToNFA .
    (Complement extendedAlphabet) .
    (parseRegexes extendedAlphabet)
