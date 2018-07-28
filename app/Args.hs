module Args
    ( Flag(..)
    , parseOptions
    ) where

import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import Data.Char ( isDigit )

data Flag = Help
          | Version
          | Matches String
          | DoesntMatch String
          | ShowMatches (Maybe Integer)
          | ShowMismatches (Maybe Integer)
          | Optimize
    deriving Show

optionalInteger :: Maybe String -> Maybe Integer
optionalInteger Nothing = Nothing
optionalInteger (Just s) = if not (all isDigit s)
                           then Nothing
                           else Just ((read s) :: Integer)

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]            (NoArg Help)                                      "Prints this help message."
    , Option ['V'] ["version"]         (NoArg Version)                                   "show version number"
    , Option ['m'] ["matches"]         (ReqArg Matches "REGEX")                          "core."
    , Option ['x'] ["doesnt-match"]    (ReqArg DoesntMatch "REGEX")                      "tier."
    , Option []    ["show-matches"]    (OptArg (ShowMatches . optionalInteger) "INT")    "Outputs all strings that match the given regex."
    , Option []    ["show-mismatches"] (OptArg (ShowMismatches . optionalInteger) "INT") "Outputs all strings that don't match the given regex."
    , Option ['O'] ["optimize"]        (NoArg Optimize)                                  "Optimizes the given regex."
    ]

parseOptions :: [String] -> Either String [Flag]
parseOptions argv =
    case getOpt Permute options argv of
        (o,n,[])   -> Right $ o ++ (map Matches n)
        (_,_,errs) -> Left $ concat errs ++ usageInfo header options
  where header = "Usage: egregex [OPTION...] regex"

