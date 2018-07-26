

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Function (on)
import Data.List (elemIndex, group, intersect, intersperse, nub, partition, sort, (\\))
import Data.Char (isAscii, isPrint)
import Data.Either (fromRight, isRight, lefts, rights)

data Term = Literal Char
          | Sequence [Term]
          | Repeat (Int, Maybe Int) Term
          | Choice [Term]
          | Set [Char]
  deriving ( Show )

data DFA state alphabetSet = DFA { dfaStates :: [state]
                                 , dfaAlphabet :: [alphabetSet]
                                 , dfaTransition :: state -> alphabetSet -> state
                                 , dfaInitialState :: state
                                 , dfaAcceptingStates :: [state] }
data NFA state alphabetSet = NFA { nfaStates :: [state]
                                 , nfaAlphabet :: [alphabetSet]
                                 , nfaTransition :: state -> Maybe alphabetSet -> [state]
                                 , nfaInitialState :: state
                                 , nfaAcceptingStates :: [state] }
data NFAOneAccepting state alphabetSet = NFAOA { nfaoaStates :: [state]
                                               , nfaoaAlphabet :: [alphabetSet]
                                               , nfaoaTransition :: state -> Maybe alphabetSet -> [state]
                                               , nfaoaInitialState :: state
                                               , nfaoaAcceptingState :: state }

data RegularGrammar nonterminal terminal = RG { nonTerminals :: [nonterminal]
                                              , startSymbol :: nonterminal
                                              , productionRules :: [(nonterminal, [Either nonterminal terminal])] }
                                          deriving (Eq)

instance (Show s, Show a) => Show (DFA s a) where
  show dfa = "states: " ++ (show (dfaStates dfa)) ++ "\n"
     ++ "alphabet: " ++ (show (dfaAlphabet dfa)) ++ "\n"
     ++ "initial state: " ++ (show (dfaInitialState dfa)) ++ "\n"
     ++ "accepting states: " ++ (show (dfaAcceptingStates dfa)) ++ "\n"
     ++ "transition function:\n"
     ++ (show (dfaAlphabet dfa)) ++ "\n"
     ++ (concat $ intersperse "\n" [show (x, map (dfaTransition dfa x) (dfaAlphabet dfa)) | x<-(dfaStates dfa)])

instance (Show s, Show a) => Show (NFA s a) where
  show nfa = "states: " ++ (show (nfaStates nfa)) ++ "\n"
     ++ "alphabet: " ++ (show (nfaAlphabet nfa)) ++ "\n"
     ++ "initial state: " ++ (show (nfaInitialState nfa)) ++ "\n"
     ++ "accepting state: " ++ (show (nfaAcceptingStates nfa)) ++ "\n"
     ++ "transition function:\n"
     ++ (show possibleTransitions) ++ "\n"
     ++ (concat $ intersperse "\n" [show (x, map (nfaTransition nfa x) possibleTransitions) | x<-(nfaStates nfa)])
    where possibleTransitions = Nothing:(map Just (nfaAlphabet nfa))

instance (Show s, Show a) => Show (NFAOneAccepting s a) where
  show nfaoa = "states: " ++ (show (nfaoaStates nfaoa)) ++ "\n"
     ++ "alphabet: " ++ (show (nfaoaAlphabet nfaoa)) ++ "\n"
     ++ "initial state: " ++ (show (nfaoaInitialState nfaoa)) ++ "\n"
     ++ "accepting state: " ++ (show (nfaoaAcceptingState nfaoa)) ++ "\n"
     ++ "transition function:\n"
     ++ (show possibleTransitions) ++ "\n"
     ++ (concat $ intersperse "\n" [show (x, map (nfaoaTransition nfaoa x) possibleTransitions) | x<-(nfaoaStates nfaoa)])
    where possibleTransitions = Nothing:(map Just (nfaoaAlphabet nfaoa))

instance (Eq nonterminal, Show nonterminal, Show terminal) => Show (RegularGrammar nonterminal terminal)  where
  show gr = "starting symbol: " ++ showNonTerminal (startSymbol gr) ++ "\n" ++ concatMap showNonTerminalRules [(nt, map snd $ filter ((==nt).fst) (productionRules gr)) | nt<-(nonTerminals gr)]
          where widthNonTerminal = 4
                arrow = " -> "
                showNonTerminalRules :: (Eq nonterminal, Show nonterminal, Show terminal) => (nonterminal, [[Either nonterminal terminal]]) -> String
                showNonTerminalRules (nt, (firstRule:rest)) = (showFirstRule nt firstRule) ++ (concatMap showRest rest)
                showNonTerminal :: Show nonterminal => nonterminal -> String
                showNonTerminal nt = '_':(show nt)
                showRule :: (Show nonterminal, Show terminal) => [Either nonterminal terminal] -> String
                showRule = foldr (\e -> \rec -> (either showNonTerminal show e) ++ (' ':rec)) ""
                showFirstRule :: (Eq nonterminal, Show nonterminal, Show terminal) => nonterminal -> [Either nonterminal terminal] -> String
                showFirstRule nt r = let s = showNonTerminal nt in s ++ (replicate (widthNonTerminal - (length s)) ' ') ++ arrow ++ (showRule r) ++ "\n"
                showRest :: (Show nonterminal, Show terminal) => [Either nonterminal terminal] -> String
                showRest r = (replicate (widthNonTerminal + (length arrow) - 2) ' ') ++ "| " ++ (showRule r) ++ "\n"


term :: Parser Term
term = buildExpressionParser ops atom where

  ops = [ [ Postfix (Repeat (0, Nothing) <$ char '*')
          , Postfix (Repeat (1, Nothing) <$ char '+')
          , Postfix (Repeat (0, Just 1)  <$ char '?')
          ]
        , [ Infix (return sequence) AssocRight
          ]
        , [ Infix (choice <$ char '|') AssocRight
          ]
        ]

  atom = msum [ Literal <$> lit
              , parens term
              ]

  lit = noneOf "*+?|()"
  sequence a b = Sequence $ (seqTerms a) ++ (seqTerms b)
  choice a b = Choice $ (choiceTerms a) ++ (choiceTerms b)
  parens = between (char '(') (char ')')

  seqTerms (Sequence ts) = ts
  seqTerms t = [t]

  choiceTerms (Choice ts) = ts
  choiceTerms t = [t]

parseRegex r = fromRight (Sequence []) (parse term "" r)

main = parseTest term "he(llo)*|wor+ld\\w?"

-- more efficient than `nub . sort` since nub does not know that the list is sorted.
nubSort :: (Ord a, Eq a) => [a] -> [a]
nubSort = map head . group . sort

-- for piece-wise transitions.
piecewiseEq :: Eq a => a -> (b -> c) -> (a -> b -> c) -> a -> b -> c
piecewiseEq a f g a' = if a == a' then f else g a

-- allChars
allChars = filter (\c -> (isPrint c) && (isAscii c)) $ enumFromTo minBound maxBound


regexToNFA :: Term -> (NFAOneAccepting Integer Char)
regexToNFA (Literal c) = NFAOA { nfaoaStates = [0, 1]
                               , nfaoaAlphabet = [c]
                               , nfaoaTransition = \state -> (\c' -> if (state == 0) && (Just c == c') then [1] else [])
                               , nfaoaInitialState = 0
                               , nfaoaAcceptingState = 1 }
regexToNFA (Sequence []) = NFAOA { nfaoaStates = [0, 1]
                                 , nfaoaAlphabet = []
                                 , nfaoaTransition = const $ const []
                                 , nfaoaInitialState = 0
                                 , nfaoaAcceptingState = 1 }
regexToNFA (Sequence [tm]) = regexToNFA tm
regexToNFA (Sequence (tm:tms)) = NFAOA { nfaoaStates = nfaoaStates nfaoa1 ++ (map (+nextState) (nfaoaStates nfaoa2))
                                       , nfaoaAlphabet = nubSort (on (++) nfaoaAlphabet nfaoa1 nfaoa2)
                                       , nfaoaTransition = \state -> \c' ->
                                           if state < nextState
                                             then nubSort ((nfaoaTransition nfaoa1 state c') ++
                                               if state == (nfaoaAcceptingState nfaoa1) && c' == Nothing
                                                 then [nextState]
                                                 else [])
                                             else map (+nextState) ((nfaoaTransition nfaoa2) (state - nextState) c')
                                       , nfaoaInitialState = nfaoaInitialState nfaoa1
                                       , nfaoaAcceptingState = (nfaoaAcceptingState nfaoa2) + nextState }
                                 where nfaoa1 = regexToNFA tm
                                       nfaoa2 = regexToNFA (Sequence tms)
                                       nextState = (maximum (nfaoaStates nfaoa1)) + 1
regexToNFA (Repeat (0, Nothing) tm) = NFAOA { nfaoaStates = (nfaoaStates nfaoa) ++ [nextState]
                                            , nfaoaAlphabet = nfaoaAlphabet nfaoa
                                            , nfaoaTransition = \state -> \c' ->
                                                if state == nextState
                                                  then []
                                                else nubSort ((transitionF state c') ++
                                                  if state == oldAcceptingS
                                                    then (if c' == Nothing then [init, nextState] else [])
                                                  else (if state == init
                                                    then (if c' == Nothing then [nextState] else [])
                                                  else []))
                                            , nfaoaInitialState = init
                                            , nfaoaAcceptingState = nextState }
                                      where nfaoa = regexToNFA tm
                                            nextState = (maximum (nfaoaStates nfaoa)) + 1
                                            oldAcceptingS = nfaoaAcceptingState nfaoa
                                            transitionF = nfaoaTransition nfaoa
                                            init = nfaoaInitialState nfaoa
regexToNFA (Repeat (n, Nothing) tm) =regexToNFA $ Sequence ((Repeat (0, Nothing) tm):(replicate n tm))



relaxOneAccepting :: NFAOneAccepting s a -> NFA s a
relaxOneAccepting x = NFA { nfaStates          = nfaoaStates x
                          , nfaAlphabet        = nfaoaAlphabet x
                          , nfaTransition      = nfaoaTransition x
                          , nfaInitialState    = nfaoaInitialState x
                          , nfaAcceptingStates = [nfaoaAcceptingState x] }

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

epsilonClosure :: (Ord s) => NFA s a -> [s] -> [s]
epsilonClosure nfa states = converge (\ss -> nubSort $ concat (ss:[nfaTransition nfa s Nothing | s<-ss])) states

determinizeNFA :: Ord s => NFA s a -> DFA [s] a
determinizeNFA nfa = DFA { dfaStates = states
                         , dfaAlphabet = nfaAlphabet nfa
                         , dfaTransition = transition
                         , dfaInitialState = initialState
                         , dfaAcceptingStates = filter (not.null.(intersect (nfaAcceptingStates nfa))) states }
                    where initialState = epsilonClosure nfa [nfaInitialState nfa]
                          (states, transition) = determinizeNFA' nfa [] (const $ const []) [initialState]

determinizeNFA' :: Ord s => NFA s a -> [[s]] -> ([s] -> a -> [s]) -> [[s]] -> ([[s]], [s] -> a -> [s])
determinizeNFA' nfa currentStates currentTransition [] = (currentStates, currentTransition)
determinizeNFA' nfa currentStates currentTransition (pss:pendingStates) =
    if elem pss currentStates
      then determinizeNFA' nfa currentStates currentTransition pendingStates
      else determinizeNFA' nfa (pss:currentStates) (\ss -> if ss == pss then psFunction else currentTransition ss) (pendingStates++(map psFunction (nfaAlphabet nfa)))
        where psFunction c = epsilonClosure nfa [x | ps<-pss, x<-(nfaTransition nfa ps (Just c))]


-- liek groupBy but for non-adjacents.
equivalenceClasses :: (a -> a -> Bool) -> [a] -> [[a]]
equivalenceClasses f [] = []
equivalenceClasses f (x:xs) = (x:(filter (f x) xs)):(equivalenceClasses f (filter (not.(f x)) xs))

indistinguishableStates :: Eq s => DFA s a -> [[s]]
indistinguishableStates dfa = converge finerIndistinguishability [states \\ acceptingStates, acceptingStates]
                             where states = dfaStates dfa
                                   acceptingStates = dfaAcceptingStates dfa
                                   delta = dfaTransition dfa
                                   alphabet = dfaAlphabet dfa
                                   finerIndistinguishability = concatMap (equivalenceClasses (\x -> \y -> all (\c -> delta x c == delta y c) alphabet))

mergeIndistinguishableStates :: Eq s => DFA s a -> [[s]] -> DFA s a
mergeIndistinguishableStates dfa equivalenceClasses = DFA {
                           dfaStates = map head equivalenceClasses
                         , dfaAlphabet = dfaAlphabet dfa
                         , dfaTransition = \s -> \c -> mergeFunction (dfaTransition dfa s c)
                         , dfaInitialState = mergeFunction (dfaInitialState dfa)
                         , dfaAcceptingStates = map mergeFunction (dfaAcceptingStates dfa) }
                where mergeFunction = mergeIndistinguishableStatesFunction dfa equivalenceClasses (dfaStates dfa)

-- base case is not defined on purpose, it should never be reached,
-- as the state should be in one of the equivalence classes.
mergeIndistinguishableStatesFunction :: Eq s => DFA s a -> [[s]] -> [s] -> s -> s
mergeIndistinguishableStatesFunction dfa equivalenceClasses (state:rest) s = if s == state
  then head (head [eqC | eqC<-equivalenceClasses, elem s eqC])
  else mergeIndistinguishableStatesFunction dfa equivalenceClasses rest s

simplifyPowersetConstruction :: Eq s => DFA [s] a -> DFA Int a
simplifyPowersetConstruction dfa = DFA {
                           dfaStates = map simplifyFunction states
                         , dfaAlphabet = dfaAlphabet dfa
                         , dfaTransition = \s -> \c -> simplifyFunction (dfaTransition dfa (states !! s) c)
                         , dfaInitialState = simplifyFunction (dfaInitialState dfa)
                         , dfaAcceptingStates = map simplifyFunction (dfaAcceptingStates dfa) }
    where states = dfaStates dfa
          primes = filter isPrime [1..]
          isPrime n = [mod n d | d<-[1..n]] == [1,n]
          simplifyFunction x = unJustify $ elemIndex x states
          unJustify (Just y) = y

minimizeDFA :: Eq s => DFA s a -> DFA s a
minimizeDFA dfa = mergeIndistinguishableStates dfa (indistinguishableStates dfa)

-- convertDFAtoGrammar :: DFA s a -> Grammar s a
convertDFAToGrammar dfa = RG { nonTerminals = states
                             , startSymbol = initialState
                             , productionRules = [(s, [Right c, Left (delta s c)]) | s<-states, c<-alpha] ++
                                                 [(s, [Right c]) | s<-states, c<-alpha, isAccepting (delta s c)] ++
                                                 (if isAccepting initialState then [(initialState, [])] else []) }
                        where states = dfaStates dfa
                              initialState = dfaInitialState dfa
                              delta = dfaTransition dfa
                              alpha = dfaAlphabet dfa
                              isAccepting = \x -> elem x (dfaAcceptingStates dfa)

rulesOfNT gr nt = map snd $ filter ((==nt).fst) (productionRules gr)

deleteSinkNonTerminals grammar = converge deleteSinkNonTerminals' grammar
    where deleteSinkNonTerminals' gr = RG { nonTerminals = (nonTerminals gr) \\ (sinkNonTerminals gr)
                                          , startSymbol = startSymbol gr
                                          , productionRules = [(nt, r) | (nt,r)<-(productionRules gr), not (elem nt (sinkNonTerminals gr)), null (lefts r) || (not $ null ((lefts r) \\ (sinkNonTerminals gr)))] }
          sinkNonTerminals gr = [nt | nt<-(nonTerminals gr), all null $ map (\x -> (lefts x) \\ (sinkNonTerminals' gr)) (rulesOfNT gr nt), not (hasBaseCase gr nt)]
          sinkNonTerminals' gr = [nt | nt<-(nonTerminals gr), not (any (hasNonTerminalOtherThanSelf nt) (rulesOfNT gr nt)), not (hasBaseCase gr nt)]
          hasBaseCase gr nt = elem [] (map lefts (rulesOfNT gr nt))
          hasNonTerminalOtherThanSelf nt = not.null.(filter (/=nt)).lefts

deleteNonRecursiveNonTerminals :: (Eq nonterminal, Eq terminal) => RegularGrammar nonterminal terminal -> RegularGrammar nonterminal terminal
deleteNonRecursiveNonTerminals = converge deleteNonRecursiveNonTerminal
deleteNonRecursiveNonTerminal grammar = if isThereANonRecursiveNT
                                     then RG { nonTerminals = (nonTerminals grammar) \\ [fstNonRecursiveNT]
                                             , startSymbol = startSymbol grammar
                                             , productionRules = [(nt, p) | (nt,p)<-(productionRules grammar), nt /= fstNonRecursiveNT, p'<-(if elem fstNonRecursiveNT (lefts p) then replaceNT p fstNonRecursiveNT (rulesOfNT grammar fstNonRecursiveNT) else [p])] }
                                     else grammar
    where nonRecursiveNTs = [nt | nt<-(nonTerminals grammar), all (not.(elem (Left nt))) (rulesOfNT grammar nt), nt /= startSymbol grammar]
          isThereANonRecursiveNT = not $ null nonRecursiveNTs
          fstNonRecursiveNT = head nonRecursiveNTs
          replaceNT [] _ _ = [[]]
          replaceNT ((Left n):ps) nt rules = if n == nt then [x++y | x<-rules, y<-(replaceNT ps n rules)] else [(Left n):y | y<-(replaceNT ps n rules)]
          replaceNT ((Right t):ps) nt rules = [(Right t):x | x<-(replaceNT ps nt rules)]



deleteOneRuleNonTerminals :: (Eq nonterminal, Eq terminal) => RegularGrammar nonterminal terminal -> RegularGrammar nonterminal terminal
deleteOneRuleNonTerminals = converge deleteOneRuleNonTerminal
deleteOneRuleNonTerminal grammar = if isThereAOneRuleNT
                                     then RG { nonTerminals = (nonTerminals grammar) \\ [fstOneRuleNT]
                                             , startSymbol = startSymbol grammar
                                             , productionRules =[(nt, if elem fstOneRuleNT (lefts p) then replaceNT p fstOneRuleNT else p) | (nt,p)<-(productionRules grammar), nt /= fstOneRuleNT] }
                                     else grammar
    where oneRuleNTs = [nt | nt<-(nonTerminals grammar), length (rulesOfNT grammar nt) == 1, nt /= startSymbol grammar]
          isThereAOneRuleNT = not $ null oneRuleNTs
          fstOneRuleNT = head oneRuleNTs
          replaceNT [] nt = []
          replaceNT ((Left n):ps) nt = (if n == nt then (head (rulesOfNT grammar n)) else [Left n])++(replaceNT ps n)
          replaceNT ((Right t):ps) nt = (Right t):(replaceNT ps nt)

unfoldGrammar grammar = RG { nonTerminals = nonTerminals grammar
                           , startSymbol = startSymbol grammar
                           , productionRules = [(nt, p') | (nt,p)<-(productionRules grammar), p'<-(applyAllRules (productionRules grammar) p)] }

optimizationStep :: (Eq nonterminal, Eq terminal)
                 => RegularGrammar nonterminal terminal -> RegularGrammar nonterminal terminal
optimizationStep = (deleteNonRecursiveNonTerminals . deleteSinkNonTerminals)

applyAllRules :: Eq nonterminal => [(nonterminal, [Either nonterminal terminal])] -> [Either nonterminal terminal] -> [[Either nonterminal terminal]]
applyAllRules prs [] = [[]]
applyAllRules prs ((Left nt):rest) = [p++rest' | p<-(map snd $ filter ((==nt).fst) prs), rest'<-(applyAllRules prs rest)]
applyAllRules prs ((Right t):rest) = [(Right t):rest' | rest'<-(applyAllRules prs rest)]


languageFromGrammar :: Eq nonterminal => RegularGrammar nonterminal terminal -> [[terminal]]
languageFromGrammar gr = languageFromGrammar' (productionRules gr) [[Left (startSymbol gr)]]


languageFromGrammar' :: Eq nonterminal => [(nonterminal, [Either nonterminal terminal])] -> [[Either nonterminal terminal]] -> [[terminal]]
languageFromGrammar' pr derivs = (map rights finished) ++ (languageFromGrammar' pr unfinished)
              where (finished, unfinished) = partition (all isRight) (concatMap (applyAllRules pr) derivs)


test1 = Sequence [Repeat (0, Nothing) (Literal 'a'), Repeat (1, Nothing) (Literal 'b')]

testF = convertDFAToGrammar . simplifyPowersetConstruction . minimizeDFA . determinizeNFA . relaxOneAccepting . regexToNFA . parseRegex
