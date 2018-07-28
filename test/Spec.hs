import EgRegexImpl


---- TESTS----
test1 = Sequence [Repeat (0, Nothing) (Literal 'a'), Repeat (1, Nothing) (Literal 'b')]

testRegexToDFA = minimizeDFA . determinizeNFA . relaxOneAccepting . regexToNFA . parseRegex
testF = convertDFAToGrammar . minimizeDFA . simplifyPowersetConstruction . determinizeNFA . relaxOneAccepting . regexToNFA . parseRegex

-- isAccepted :: DFA s a -> [a] -> Bool
isAccepted dfa str = isAccepted' dfa str (dfaInitialState dfa)
  where
    isAccepted' dfa [] s = s `elem` dfaAcceptingStates dfa
    isAccepted' dfa (x:xs) s = isAccepted' dfa xs (dfaTransition dfa s x)

testDFA1 = accepts "aaaac" && accepts "abbbbbbc" && not (accepts "ab")
  where
    dfa1 = testRegexToDFA "a+b*c+"
    accepts = isAccepted dfa1


main :: IO ()
main = putStrLn (if testDFA1 then "Test passed" else "Test failed")
