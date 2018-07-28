module EgRegexImpl where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Function (on)
import Data.List
    ( elemIndex
    , group
    , intersect
    , intercalate
    , intersperse
    , maximumBy
    , nub
    , partition
    , sort
    , sortBy
    , (\\)
    )
import Data.Char (isAscii, isPrint)
import Data.Either (fromRight, isRight, lefts, rights)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set (member)

import Debug.Trace

data Term = Literal Char
          | Sequence [Term]
          | Repeat (Int, Maybe Int) Term
          | Choice [Term]
          | CharSet (Set Char)
          | Intersect [Term]
          | Complement [Char] Term
  deriving ( Show )

data DFA state alphabetSet = DFA
    { dfaStates :: [state]
    , dfaAlphabet :: [alphabetSet]
    , dfaTransition :: state -> alphabetSet -> state
    , dfaInitialState :: state
    , dfaAcceptingStates :: [state]
    }

data NFA state alphabetSet = NFA
    { nfaStates :: [state]
    , nfaAlphabet :: [alphabetSet]
    , nfaTransition :: state -> Maybe alphabetSet -> [state]
    , nfaInitialState :: state
    , nfaAcceptingStates :: [state]
    }

data NFAOneAccepting state alphabetSet = NFAOA
    { nfaoaStates :: [state]
    , nfaoaAlphabet :: [alphabetSet]
    , nfaoaTransition :: state -> Maybe alphabetSet -> [state]
    , nfaoaInitialState :: state
    , nfaoaAcceptingState :: state
    }

data RegularGrammar nonterminal terminal = RG
    { nonTerminals :: [nonterminal]
    , startSymbol :: nonterminal
    , productionRules :: [(nonterminal, [[Either nonterminal terminal]])]
    }
  deriving (Eq)

instance (Show s, Show a) => Show (DFA s a) where
  show dfa = "states: " ++ show (dfaStates dfa) ++ "\n"
     ++ "alphabet: " ++ show (dfaAlphabet dfa) ++ "\n"
     ++ "initial state: " ++ show (dfaInitialState dfa) ++ "\n"
     ++ "accepting states: " ++ show (dfaAcceptingStates dfa) ++ "\n"
     ++ "transition function:\n"
     ++ show (dfaAlphabet dfa) ++ "\n"
     ++ intercalate "\n" [show (x, map (dfaTransition dfa x) (dfaAlphabet dfa)) | x <- dfaStates dfa]

instance (Show s, Show a) => Show (NFA s a) where
  show nfa = "states: " ++ show states ++ "\n"
     ++ "alphabet: " ++ show (nfaAlphabet nfa) ++ "\n"
     ++ "initial state: " ++ show (nfaInitialState nfa) ++ "\n"
     ++ "accepting state: " ++ show (nfaAcceptingStates nfa) ++ "\n"
     ++ "transition function:\n"
     ++ show possibleTransitions ++ "\n"
     ++ intercalate "\n" [show (x, map (nfaTransition nfa x) possibleTransitions) | x <- states]
    where possibleTransitions = Nothing : map Just (nfaAlphabet nfa)
          states = nfaStates nfa

instance (Show s, Show a) => Show (NFAOneAccepting s a) where
  show nfaoa = "states: " ++ show states ++ "\n"
     ++ "alphabet: " ++ show (nfaoaAlphabet nfaoa) ++ "\n"
     ++ "initial state: " ++ show (nfaoaInitialState nfaoa) ++ "\n"
     ++ "accepting state: " ++ show (nfaoaAcceptingState nfaoa) ++ "\n"
     ++ "transition function:\n"
     ++ show possibleTransitions ++ "\n"
     ++ intercalate "\n" [show (x, map (nfaoaTransition nfaoa x) possibleTransitions) | x <- states]
    where possibleTransitions = Nothing : map Just (nfaoaAlphabet nfaoa)
          states = nfaoaStates nfaoa

-- [(nt, map snd $ filter ((==nt).fst) (productionRules gr)) | nt<-(nonTerminals gr)]
instance (Eq nonterminal, Show nonterminal, Show terminal) => Show (RegularGrammar nonterminal terminal)  where
    show gr = "starting symbol: " ++ showNonTerminal (startSymbol gr) ++ "\n" ++
           "nonterminals: " ++ show (nonTerminals gr) ++ "\n" ++
           concatMap showNonTerminalRules (productionRules gr)
      where
        widthNonTerminal = 4
        arrow = " -> "
        showNonTerminalRules :: (Eq nonterminal, Show nonterminal, Show terminal) => (nonterminal, [[Either nonterminal terminal]]) -> String
        showNonTerminalRules (nt, firstRule:rest) = showFirstRule nt firstRule ++ concatMap showRest rest
        showNonTerminalRules (nt, []) = showNonTerminal nt ++ " -> ERROR\n"
        showNonTerminal :: Show nonterminal => nonterminal -> String
        showNonTerminal nt = '_' : show nt
        showRule :: (Show nonterminal, Show terminal) => [Either nonterminal terminal] -> String
        showRule = foldr (\e rec -> either showNonTerminal show e ++ (' ':rec)) ""
        showFirstRule :: (Eq nonterminal, Show nonterminal, Show terminal) => nonterminal -> [Either nonterminal terminal] -> String
        showFirstRule nt r = let s = showNonTerminal nt in s ++ replicate (widthNonTerminal - length s) ' ' ++ arrow ++ showRule r ++ "\n"
        showRest :: (Show nonterminal, Show terminal) => [Either nonterminal terminal] -> String
        showRest r = replicate (widthNonTerminal + length arrow - 2) ' ' ++ "| " ++ showRule r ++ "\n"


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
  sequence a b = Sequence $ seqTerms a ++ seqTerms b
  choice a b = Choice $ choiceTerms a ++ choiceTerms b
  parens = between (char '(') (char ')')

  seqTerms (Sequence ts) = ts
  seqTerms t = [t]

  choiceTerms (Choice ts) = ts
  choiceTerms t = [t]

parseRegex r = fromRight (Sequence []) (parse term "" r)



--------------------------------------------------------------------------------
--- General utilities ----------------------------------------------------------
--------------------------------------------------------------------------------
--
addIf g y xs = if g then y:xs else xs

-- more efficient than `nub . sort` since nub does not know that the list is sorted.
nubSort :: (Ord a, Eq a) => [a] -> [a]
nubSort = map head . group . sort

-- for piece-wise transitions.
piecewiseEq :: Eq a => a -> (b -> c) -> (a -> b -> c) -> a -> b -> c
piecewiseEq a f g a' = if a == a' then f else g a

-- allChars
allChars = filter (\c -> isPrint c && isAscii c) $ enumFromTo minBound maxBound

parseRegexes :: [Char] -> [Either String String] -> Term
parseRegexes extendedAlphabet ss = if length regexes == 1
                                   then head regexes
                                   else Intersect regexes
  where
    regexes = map (either ((Complement extendedAlphabet).parseRegex) parseRegex) ss

--------------------------------------------------------------------------------
--- Obtaining the non-deterministic finite automaton ---------------------------
--------------------------------------------------------------------------------

adjustNFAOA :: Integer -> NFAOneAccepting Integer c -> NFAOneAccepting Integer c
adjustNFAOA n nfaoa = NFAOA
    { nfaoaStates = map (+n) (nfaoaStates nfaoa)
    , nfaoaAlphabet = nfaoaAlphabet nfaoa
    , nfaoaTransition = \state c' -> map (+n) ((nfaoaTransition nfaoa) (state - n) c')
    , nfaoaInitialState = n + (nfaoaInitialState nfaoa)
    , nfaoaAcceptingState = n + (nfaoaAcceptingState nfaoa)
    }

regexToNFA :: Term -> NFAOneAccepting Integer Char
regexToNFA (Literal c) = NFAOA
    { nfaoaStates = [0, 1]
    , nfaoaAlphabet = [c]
    , nfaoaTransition = \state c' -> [1 | (state == 0) && (Just c == c')]
    , nfaoaInitialState = 0
    , nfaoaAcceptingState = 1
    }
regexToNFA (Sequence []) = NFAOA
    { nfaoaStates = [0, 1]
    , nfaoaAlphabet = []
    , nfaoaTransition = const $ const []
    , nfaoaInitialState = 0
    , nfaoaAcceptingState = 1
    }
regexToNFA (Sequence [tm]) = regexToNFA tm
regexToNFA (Sequence (tm:tms)) = NFAOA
    { nfaoaStates = nfaoaStates nfaoa1 ++ map (+nextState) (nfaoaStates nfaoa2)
    , nfaoaAlphabet = nubSort (on (++) nfaoaAlphabet nfaoa1 nfaoa2)
    , nfaoaTransition = \state c' ->
        if state < nextState
          then nubSort (addIf (state == acceptingState1 && isNothing c') nextState (delta1 state c'))
          else map (+nextState) (nfaoaTransition nfaoa2 (state - nextState) c')
    , nfaoaInitialState = nfaoaInitialState nfaoa1
    , nfaoaAcceptingState = nfaoaAcceptingState nfaoa2 + nextState
    }
  where
    nfaoa1 = regexToNFA tm
    nfaoa2 = regexToNFA (Sequence tms)
    acceptingState1 = nfaoaAcceptingState nfaoa1
    delta1 = nfaoaTransition nfaoa1
    nextState = maximum (nfaoaStates nfaoa1) + 1
regexToNFA (Repeat (0, Nothing) tm) = NFAOA
    { nfaoaStates = nfaoaStates nfaoa ++ [nextState]
    , nfaoaAlphabet = nfaoaAlphabet nfaoa
    , nfaoaTransition = \state c' ->
        if state == nextState
        then []
        else nubSort (transitionF state c' ++
            if state == oldAcceptingS
                then (if isNothing c' then [init, nextState] else [])
            else (if state == init
                then [nextState | isNothing c']
            else []))
    , nfaoaInitialState = init
    , nfaoaAcceptingState = nextState
    }
  where
    nfaoa = regexToNFA tm
    nextState = maximum (nfaoaStates nfaoa) + 1
    oldAcceptingS = nfaoaAcceptingState nfaoa
    transitionF = nfaoaTransition nfaoa
    init = nfaoaInitialState nfaoa
regexToNFA (Repeat (n, Nothing) tm) = regexToNFA $ Sequence (Repeat (0, Nothing) tm : replicate n tm)
regexToNFA (Choice []) = regexToNFA (Sequence [])
regexToNFA (Choice xs) = NFAOA
    { nfaoaStates = sort (0 : nextState : (foldl1 (++) (map nfaoaStates adjustedNFAOAs)))
    , nfaoaAlphabet = nubSort (foldl1 (++) (map nfaoaAlphabet adjustedNFAOAs))
    , nfaoaTransition = \state c' ->
        if state == 0
            then (if c' == Nothing then init newStartingStates else [])
        else (if state == nextState
            then []
        else (nfaoaTransition (corresponding newStartingStates adjustedNFAOAs state) state c')) ++
          [nextState | state `elem` acceptingStates && isNothing c']
    , nfaoaInitialState = 0
    , nfaoaAcceptingState = nextState
    }
  where
    nfaoas = map regexToNFA xs
    lengths = map (toInteger . length . nfaoaStates) nfaoas
    newStartingStates = 1 : (map (+1) (scanl1 (+) lengths))
    adjustedNFAOAs = zipWith adjustNFAOA newStartingStates nfaoas
    nextState = last newStartingStates
    corresponding (i:i':is) (nfa':nfas') i'' = if i <= i'' && i'' < i'
                                               then nfa' else corresponding (i':is) nfas' i''
    acceptingStates = map nfaoaAcceptingState adjustedNFAOAs
regexToNFA (Complement newAlphabet tm) = NFAOA
    { nfaoaStates = sort (newAccepting : (dfaStates cDFA))
    , nfaoaAlphabet = dfaAlphabet cDFA
    , nfaoaTransition = \state c' ->
        if state == newAccepting
        then [newAccepting | isNothing c']
        else (case c' of
            Nothing  -> [newAccepting | state `elem` (dfaAcceptingStates cDFA)]
            Just c'' -> [dfaTransition cDFA state c''])
    , nfaoaInitialState = dfaInitialState cDFA
    , nfaoaAcceptingState = newAccepting
    }
  where
    determinization = simplifyPowersetConstruction . determinizeNFA . relaxOneAccepting
    cDFA = complementDFA newAlphabet (determinization (regexToNFA tm))
    newAccepting = maximum (dfaStates cDFA) + 1



relaxOneAccepting :: NFAOneAccepting s a -> NFA s a
relaxOneAccepting x = NFA { nfaStates          = nfaoaStates x
                          , nfaAlphabet        = nfaoaAlphabet x
                          , nfaTransition      = nfaoaTransition x
                          , nfaInitialState    = nfaoaInitialState x
                          , nfaAcceptingStates = [nfaoaAcceptingState x]
                          }

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

epsilonClosure :: (Ord s) => NFA s a -> [s] -> [s]
epsilonClosure nfa = converge (\ss -> nubSort $ concat (ss:[nfaTransition nfa s Nothing | s<-ss]))

--------------------------------------------------------------------------------
--- Obtaining the deterministic finite automaton -------------------------------
--------------------------------------------------------------------------------

determinizeNFA :: Ord s => NFA s a -> DFA [s] a
determinizeNFA nfa = DFA
    { dfaStates = states
    , dfaAlphabet = nfaAlphabet nfa
    , dfaTransition = transition
    , dfaInitialState = initialState
    , dfaAcceptingStates = filter (not . null . intersect (nfaAcceptingStates nfa)) states
    }
  where
    initialState = epsilonClosure nfa [nfaInitialState nfa]
    (states, transition) = determinizeNFA' nfa [] (const $ const []) [initialState]

determinizeNFA' :: Ord s => NFA s a -> [[s]] -> ([s] -> a -> [s]) -> [[s]] -> ([[s]], [s] -> a -> [s])
determinizeNFA' nfa currentStates currentTransition [] = (nub currentStates, currentTransition)
determinizeNFA' nfa currentStates currentTransition (pss:pendingStates) =
    if pss `elem` currentStates
    then determinizeNFA' nfa currentStates currentTransition pendingStates
    else determinizeNFA' nfa (pss:currentStates)
        (\ss -> if ss == pss then psFunction else currentTransition ss)
        (pendingStates ++ map psFunction (nfaAlphabet nfa))
  where
    psFunction c = epsilonClosure nfa [x | ps<-pss, x <- nfaTransition nfa ps (Just c)]


-- liek groupBy but for non-adjacents.
equivalenceClasses :: (a -> a -> Bool) -> [a] -> [[a]]
equivalenceClasses f [] = []
equivalenceClasses f (x:xs) = (x : filter (f x) xs) : equivalenceClasses f (filter (not. f x) xs)

indistinguishableStates :: Eq s => DFA s a -> [[s]]
indistinguishableStates dfa = converge finerIndistinguishability [states \\ acceptingStates, acceptingStates]
  where
    states = dfaStates dfa
    acceptingStates = dfaAcceptingStates dfa
    delta = dfaTransition dfa
    alphabet = dfaAlphabet dfa
    finerIndistinguishability = concatMap (equivalenceClasses (\x y -> all (\c -> delta x c == delta y c) alphabet))

mergeIndistinguishableStates :: Eq s => DFA s a -> [[s]] -> DFA s a
mergeIndistinguishableStates dfa equivalenceClasses = DFA
    { dfaStates = map head equivalenceClasses
    , dfaAlphabet = dfaAlphabet dfa
    , dfaTransition = \s -> mergeFunction . dfaTransition dfa s
    , dfaInitialState = mergeFunction (dfaInitialState dfa)
    , dfaAcceptingStates = nub $ map mergeFunction (dfaAcceptingStates dfa)
    }
  where
    mergeFunction = mergeIndistinguishableStatesFunction dfa equivalenceClasses (dfaStates dfa)

-- base case is not defined on purpose, it should never be reached,
-- as the state should be in one of the equivalence classes.
mergeIndistinguishableStatesFunction :: Eq s => DFA s a -> [[s]] -> [s] -> s -> s
mergeIndistinguishableStatesFunction dfa equivalenceClasses (state:rest) s =
  if s == state
  then head (head [eqC | eqC<-equivalenceClasses, s `elem` eqC])
  else mergeIndistinguishableStatesFunction dfa equivalenceClasses rest s

simplifyPowersetConstruction :: Eq s => DFA [s] a -> DFA Integer a
simplifyPowersetConstruction dfa = DFA
    { dfaStates = map simplifyFunction states
    , dfaAlphabet = dfaAlphabet dfa
    , dfaTransition = \s c ->
          simplifyFunction (dfaTransition dfa (states !! (fromInteger s)) c)
    , dfaInitialState = simplifyFunction (dfaInitialState dfa)
    , dfaAcceptingStates = map simplifyFunction (dfaAcceptingStates dfa)
    }
  where
    states = dfaStates dfa
    simplifyFunction x = toInteger $ unJustify $ elemIndex x states
    unJustify (Just y) = y

minimizeDFA :: Eq s => DFA s a -> DFA s a
minimizeDFA dfa = mergeIndistinguishableStates dfa (indistinguishableStates dfa)

--------------------------------------------------------------------------------
--- Obtaining the grammar ------------------------------------------------------
--------------------------------------------------------------------------------

groupRulesByNonTerminal :: Eq nonterminal => [nonterminal] -> [(nonterminal, [Either nonterminal terminal])] -> [(nonterminal, [[Either nonterminal terminal]])]
groupRulesByNonTerminal nts rs = [(nt, map snd $ filter ((==nt).fst) rs) | nt<-nts]

-- convertDFAtoGrammar :: DFA s a -> Grammar s a
convertDFAToGrammar dfa = RG
    { nonTerminals = states
    , startSymbol = initialState
    , productionRules = groupRulesByNonTerminal states (
        [(s, [Right c, Left (delta s c)]) | s<-states, c<-alpha] ++
        [(s, [Right c]) | s<-states, c<-alpha, isAccepting (delta s c)] ++
        [(initialState, []) | isAccepting initialState])
    }
  where
    states = dfaStates dfa
    initialState = dfaInitialState dfa
    delta = dfaTransition dfa
    alpha = dfaAlphabet dfa
    isAccepting x = x `elem` dfaAcceptingStates dfa

rulesOfNT gr nt = fromMaybe [] (lookup nt (productionRules gr))

deleteSinkNonTerminals :: (Eq nonterminal, Eq terminal) => RegularGrammar nonterminal terminal
                       -> RegularGrammar nonterminal terminal
deleteSinkNonTerminals = converge deleteSinkNonTerminals'
  where
    deleteSinkNonTerminals' gr = RG
        { nonTerminals = nonTerminals gr \\ sinkNonTerminals gr
        , startSymbol = startSymbol gr
        , productionRules = [(nt, [r | r<-rs, null (lefts r) || not (null (lefts r \\ sinkNonTerminals gr))]) | (nt,rs) <- productionRules gr, nt `notElem` sinkNonTerminals gr]
        }
    sinkNonTerminals gr = [nt | nt <- nonTerminals gr, all null $ map (\x -> lefts x \\ sinkNonTerminals' gr) (rulesOfNT gr nt), not (hasBaseCase gr nt)]
    sinkNonTerminals' gr = [nt | nt <- nonTerminals gr, not (any (hasNonTerminalOtherThanSelf nt) (rulesOfNT gr nt)), not (hasBaseCase gr nt)]
    hasBaseCase gr nt = [] `elem` map lefts (rulesOfNT gr nt)
    hasNonTerminalOtherThanSelf nt = any (/=nt) . lefts

deleteNonRecursiveNonTerminals :: (Eq nonterminal, Eq terminal) => RegularGrammar nonterminal terminal -> RegularGrammar nonterminal terminal
deleteNonRecursiveNonTerminals = converge deleteNonRecursiveNonTerminal
deleteNonRecursiveNonTerminal grammar =
    if isThereANonRecursiveNT
    then RG
        { nonTerminals = nonTerminals grammar \\ [fstNonRecursiveNT]
        , startSymbol = startSymbol grammar
        , productionRules = [(nt, concat [if fstNonRecursiveNT `elem` lefts p' then replaceNT p' fstNonRecursiveNT fstNonRecursiveNTRules else [p'] | p'<-p]) | (nt,p) <- productionRules grammar, nt /= fstNonRecursiveNT]
        }
    else grammar
  where
    nonRecursiveNTs = [nt | nt <- nonTerminals grammar, all (notElem (Left nt)) (rulesOfNT grammar nt), nt /= startSymbol grammar]
    isThereANonRecursiveNT = not $ null nonRecursiveNTs
    fstNonRecursiveNT = head nonRecursiveNTs
    fstNonRecursiveNTRules = rulesOfNT grammar fstNonRecursiveNT
    replaceNT [] _ _ = [[]]
    replaceNT (Left n : ps) nt rules =
        if n == nt
        then [x++y | x<-rules, y <- replaceNT ps n rules]
        else [Left n : y | y <- replaceNT ps n rules]
    replaceNT (Right t : ps) nt rules = [Right t : x | x <- replaceNT ps nt rules]

isSublist (x:xs) [] = False
isSublist (x:xs) ys = (x `elem` ys) && (isSublist xs ys)
isSublist [] _ = True

deleteSameRules :: (Eq nonterminal, Eq terminal) => RegularGrammar nonterminal terminal -> RegularGrammar nonterminal terminal
deleteSameRules grammar = converge deleteSameRules' grammar

deleteSameRules' grammar =
    if length sameRules == 1
    then grammar
    else RG
        { nonTerminals = newNonTerminals
        , startSymbol = startS
        , productionRules = [(nt, map (map (either transformRestNTs Right)) p) | (nt,p) <- rules, nt `notElem` restNTs]
        }
  where
    rules = productionRules grammar
    sameRules = maximumBy (comparing length)
                          (map (map fst) (equivalenceClasses (eqList `on` snd) rules))
    eqList xs ys = (isSublist xs ys) && (isSublist ys xs)
    startS = startSymbol grammar
    representantNT = if startS `elem` sameRules then startS else head sameRules
    restNTs = sameRules \\ [representantNT]
    newNonTerminals = (nonTerminals grammar) \\ restNTs
    transformRestNTs x = Left (if x `elem` restNTs then representantNT else x)

-- deleteIncludedRules :: (Eq nonterminal, Eq terminal) => RegularGrammar nonterminal terminal -> RegularGrammar nonterminal terminal
deleteIncludedRules grammar = converge deleteIncludedRules' grammar

-- deleteIncludedRules' :: (Eq nonterminal, Eq terminal) => RegularGrammar nonterminal terminal -> RegularGrammar nonterminal terminal
deleteIncludedRules' grammar = maybe grammar (replaceIncluded grammar) (findIncludedRules (productionRules grammar))
  where
    findIncludedRules rules =
        let inc = filter (\(_,_,z) -> length z > 1) [(nt, ps, [nt' | (nt', ps') <- rules, ps `isSublist` ps']) | (nt, ps) <- rules]
        in (if null inc
            then Nothing
            else Just (head (sortBy (flip (comparing (\(_,ps'',_) -> length ps''))) inc)))
    replaceIncluded gr (minNT, minPs, ntsReplace) = RG
        { nonTerminals = nonTerminals gr
        , startSymbol = startSymbol gr
        , productionRules = [(nt, if nt `elem` ntsReplace && nt /= minNT then [Left minNT]:(p \\ minPs) else p) | (nt,p) <- productionRules gr]
        }

-- normalizeGrammar :: Ord nonterminal
--                  => RegularGrammar nonterminal terminal -> RegularGrammar nonterminal terminal
-- normalizeGrammar gr = RG
--     { nonTerminals = nonTerminals gr
--     , startSymbol = startSymbol gr
--     , productionRules = (rulesOfNT gr (startSymbol gr)) : [
--           | nt <- restNonTerminals
--       ]
--     }
--   where
--     restNonTerminals = sort (nonTerminals gr \\ [startSymbol gr])
--     newNonTerminals = (startSymbol gr) : restNonTerminals
--     isBefore nt nt' = (elemIndex nt newNonTerminals) < (elemIndex nt newNonTerminals)


optimizeGrammar :: (Eq nonterminal, Eq terminal)
                => RegularGrammar nonterminal terminal -> RegularGrammar nonterminal terminal
optimizeGrammar = converge $ deleteIncludedRules . deleteSameRules . deleteNonRecursiveNonTerminals . deleteSinkNonTerminals

--------------------------------------------------------------------------------
-- Obtaining the complement DFA ------------------------------------------------
--------------------------------------------------------------------------------

extendDFA :: (Num state, Ord state, Eq alphabetSet)
          => [alphabetSet] -> DFA state alphabetSet -> DFA state alphabetSet
extendDFA newAlphabet dfa =
    if newAlphabet `isSublist` (dfaAlphabet dfa)
    then dfa
    else DFA
        { dfaStates = newStates
        , dfaAlphabet = dfaAlphabet dfa ++ newAlphabet'
        , dfaTransition = \state c' -> if state == sinkState
                                       then sinkState
                                       else (if c' `elem` (dfaAlphabet dfa)
                                             then dfaTransition dfa state c'
                                             else sinkState)
        , dfaInitialState = dfaInitialState dfa
        , dfaAcceptingStates = dfaAcceptingStates dfa
        }
  where
    sinkState = maximum (dfaStates dfa) + 1
    newAlphabet' = newAlphabet \\ dfaAlphabet dfa
    newStates = sort (sinkState : (dfaStates dfa))

complementDFA newAlphabet dfa = let dfa'= extendDFA newAlphabet dfa in DFA
    { dfaStates = dfaStates dfa'
    , dfaAlphabet = dfaAlphabet dfa'
    , dfaTransition = dfaTransition dfa'
    , dfaInitialState = dfaInitialState dfa'
    , dfaAcceptingStates = (dfaStates dfa') \\ (dfaAcceptingStates dfa')
    }



--------------------------------------------------------------------------------
-- Obtaining an optimized regular expression -----------------------------------
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Obtaining the language ------------------------------------------------------
--------------------------------------------------------------------------------

applyAllRules :: Eq nonterminal
              => [(nonterminal, [[Either nonterminal terminal]])]
              -> [Either nonterminal terminal]
              -> [[Either nonterminal terminal]]
applyAllRules prs [] = [[]]
applyAllRules prs (Left nt : rest) =
    [p ++ rest' | p <- concatMap snd (filter ((==nt).fst) prs), rest' <- applyAllRules prs rest]
applyAllRules prs (Right t : rest) = [Right t : rest' | rest' <- applyAllRules prs rest]


languageFromGrammar :: Eq nonterminal => RegularGrammar nonterminal terminal -> [[terminal]]
languageFromGrammar gr = languageFromGrammar' (productionRules gr) [[Left (startSymbol gr)]]


languageFromGrammar' :: Eq nonterminal
                     => [(nonterminal, [[Either nonterminal terminal]])]
                     -> [[Either nonterminal terminal]]
                     -> [[terminal]]
languageFromGrammar' pr [] = []
languageFromGrammar' pr derivs = map rights finished ++ languageFromGrammar' pr unfinished
  where
    (finished, unfinished) = partition (all isRight) (concatMap (applyAllRules pr) derivs)
