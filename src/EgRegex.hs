module EgRegex
    ( languageFromRegexString
    , complementLanguageFromRegexString
    ) where

import EgRegexImpl

languageFromRegexString :: String -> [String]
languageFromRegexString = languageFromGrammar .
                          optimizeGrammar .
                          convertDFAToGrammar .
                          simplifyPowersetConstruction .
                          minimizeDFA .
                          determinizeNFA .
                          relaxOneAccepting .
                          regexToNFA .
                          parseRegex

complementLanguageFromRegexString :: String -> [String]
complementLanguageFromRegexString _ = []
