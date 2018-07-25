

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Function (on)
import Data.List (nub)

data Term = Literal Char
          | Sequence [Term]
          | Repeat (Int, Maybe Int) Term
          | Choice [Term]
          | Set [Char]
  deriving ( Show )

data DFA state alphabetSet = DFA { dfaStates :: [state]
                                 , dfaAlphabet :: [alphabetSet]
                                 , dfaTransition :: state -> alphabetSet -> Maybe state
                                 , dfaInitialState :: state
                                 , dfaAcceptingStates :: [state] }
data NFA state alphabetSet = NFA { nfaStates :: [state]
                                 , nfaAlphabet :: [alphabetSet]
                                 , nfaTransition :: state -> Maybe alphabetSet -> Maybe state
                                 , nfaInitialState :: state
                                 , nfaAcceptingStates :: [state] }
data NFAOneAccepting state alphabetSet = NFAOA { nfaoaStates :: [state]
                                               , nfaoaAlphabet :: [alphabetSet]
                                               , nfaoaTransition :: state -> Maybe alphabetSet -> Maybe state
                                               , nfaoaInitialState :: state
                                               , nfaoaAcceptingState :: state }


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


regexToNFA :: Term -> (NFAOneAccepting Integer Char)
regexToNFA (Literal c) = NFAOA { nfaoaStates = [0, 1]
                               , nfaoaAlphabet = [c]
                               , nfaoaTransition = \state -> (\c' -> if (state == 0) && (Just c == c') then Just 1 else Nothing)
                               , nfaoaInitialState = 0
                               , nfaoaAcceptingState = 1 }
regexToNFA (Sequence []) = NFAOA { nfaoaStates = [0, 1]
                                 , nfaoaAlphabet = []
                                 , nfaoaTransition = const $ const Nothing
                                 , nfaoaInitialState = 0
                                 , nfaoaAcceptingState = 1 }
regexToNFA (Sequence (tm:tms)) = NFAOA { nfaoaStates = nfaoaStates nfaoa1 ++ (map (+maxState1) (nfaoaStates  nfaoa1))
                                       , nfaoaAlphabet = nub (on (++) nfaoaAlphabet nfaoa1 nfaoa2)
                                       , nfaoaTransition = \state -> \c' -> if state <= maxState1 then nfaoaTransition nfaoa1 state c' else fmap (+maxState1) ((nfaoaTransition nfaoa2) (state - maxState1) c')
                                       , nfaoaInitialState = 0
                                       , nfaoaAcceptingState = (nfaoaAcceptingState nfaoa2) + maxState1}
                                 where nfaoa1 = regexToNFA tm
                                       nfaoa2 = regexToNFA (Sequence tms)
                                       maxState1 = maximum (nfaoaStates nfaoa1)



