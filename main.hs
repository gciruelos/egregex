

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Function (on)
import Data.List (group, intersect, intersperse, nub, sort)
import Data.Char (isAscii, isPrint)

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

instance (Show s, Show a) => Show (DFA s a) where
  show dfa = "states: " ++ (show (dfaStates dfa)) ++ "\n"
     ++ "alphabet: " ++ (show (dfaAlphabet dfa)) ++ "\n"
     ++ "initial state: " ++ (show (dfaInitialState dfa)) ++ "\n"
     ++ "accepting states: " ++ (show (dfaAcceptingStates dfa)) ++ "\n"
     ++ "transition function:\n"
     ++ (show (dfaAlphabet dfa)) ++ "\n"
     ++ (concat $ intersperse "\n" [show (x, map (dfaTransition dfa x) (dfaAlphabet dfa)) | x<-(dfaStates dfa)])

instance (Show s, Show a) => Show (NFAOneAccepting s a) where
  show nfaoa = "states: " ++ (show (nfaoaStates nfaoa)) ++ "\n"
     ++ "alphabet: " ++ (show (nfaoaAlphabet nfaoa)) ++ "\n"
     ++ "initial state: " ++ (show (nfaoaInitialState nfaoa)) ++ "\n"
     ++ "accepting state: " ++ (show (nfaoaAcceptingState nfaoa)) ++ "\n"
     ++ "transition function:\n"
     ++ (show possibleTransitions) ++ "\n"
     ++ (concat $ intersperse "\n" [show (x, map (nfaoaTransition nfaoa x) possibleTransitions) | x<-(nfaoaStates nfaoa)])
    where possibleTransitions = Nothing:(map Just (nfaoaAlphabet nfaoa))


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

epsilonClosure :: (Ord s) => NFA s a -> [s] -> [s]
epsilonClosure nfa states = epsilonClosure' $ nubSort states
                            where epsilonClosure' sts = let sts2 = nubSort $ concat (sts:[concatMap (transitionFunction x) possibleTransitions | x<-sts])
                                                        in if sts == sts2 then sts2 else epsilonClosure' sts2
                                  transitionFunction = nfaTransition nfa
                                  possibleTransitions = Nothing:(map Just (nfaAlphabet nfa))

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
      else determinizeNFA' nfa (pss:currentStates) (piecewiseEq pss psFunction currentTransition) (pendingStates++(map psFunction (nfaAlphabet nfa)))
        where psFunction c = epsilonClosure nfa [x | ps<-pss, x<-(nfaTransition nfa ps (Just c))]
