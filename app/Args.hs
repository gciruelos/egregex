module Args
    ( Flag(..)
    , parseOptions
    , showHelp
    ) where

import System.Console.GetOpt
import Data.Char ( isDigit )

data Flag = Help
          | Version
          | Matches String
          | DoesntMatch String
          | ShowMatches (Maybe Integer)
          | ShowMismatches (Maybe Integer)
          | Optimize (Maybe Integer)
    deriving (Eq, Show)

optionalInteger :: Maybe String -> Maybe Integer
optionalInteger Nothing = Nothing
optionalInteger (Just s) = if not (all isDigit s)
                           then Nothing
                           else Just ((read s) :: Integer)

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]            (NoArg Help)                                      "Prints this help message."
    , Option ['V'] ["version"]         (NoArg Version)                                   "Show version number"
    , Option ['m'] ["matches"]         (ReqArg Matches "REGEX")                          "Match the given regex."
    , Option ['x'] ["doesnt-match"]    (ReqArg DoesntMatch "REGEX")                      "Don't match the given regex."
    , Option []    ["show-matches"]    (OptArg (ShowMatches . optionalInteger) "INT")    "Outputs all strings that match the given regex."
    , Option []    ["show-mismatches"] (OptArg (ShowMismatches . optionalInteger) "INT") "Outputs all strings that don't match the given regex."
    , Option ['O'] ["optimize"]        (OptArg (Optimize . optionalInteger) "INT")       "Optimizes the given regex."
    ]

showHelp :: [String] -> String
showHelp errs =  concat errs ++ usageInfo header options
  where header = "Usage: egregex [OPTION...] [regex]"


parseOptions :: [String] -> Either String [Flag]
parseOptions argv =
    case getOpt Permute options argv of
        (o,n,[])   -> Right $ o ++ (map Matches n)
        (_,_,errs) -> Left $ showHelp errs

