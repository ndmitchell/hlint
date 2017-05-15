
module Config.Type(
    Severity(..), Classify(..), HintRule(..), Note(..), Setting(..),
    Restrict(..), RestrictType(..),
    defaultHintName, isUnifyVar, showNotes, getSeverity, getRestrictType
    ) where

import HSE.All
import Data.Char
import Data.List.Extra
import Prelude


getSeverity :: String -> Maybe Severity
getSeverity "ignore" = Just Ignore
getSeverity "warn" = Just Warning
getSeverity "warning" = Just Warning
getSeverity "suggest" = Just Suggestion
getSeverity "suggestion" = Just Suggestion
getSeverity "error" = Just Error
getSeverity "hint" = Just Suggestion
getSeverity _ = Nothing

getRestrictType :: String -> Maybe RestrictType
getRestrictType "modules" = Just RestrictModule
getRestrictType "extensions" = Just RestrictExtension
getRestrictType "flags" = Just RestrictFlag
getRestrictType "functions" = Just RestrictFunction
getRestrictType _ = Nothing

defaultHintName :: String
defaultHintName = "Use alternative"


-- | How severe an issue is.
data Severity
    = Ignore -- ^ The issue has been explicitly ignored and will usually be hidden (pass @--show@ on the command line to see ignored ideas).
    | Suggestion -- ^ Suggestions are things that some people may consider improvements, but some may not.
    | Warning -- ^ Warnings are suggestions that are nearly always a good idea to apply.
    | Error -- ^ Available as a setting for the user.
      deriving (Eq,Ord,Show,Read,Bounded,Enum)


-- Any 1-letter variable names are assumed to be unification variables
isUnifyVar :: String -> Bool
isUnifyVar [x] = x == '?' || isAlpha x
isUnifyVar _ = False


---------------------------------------------------------------------
-- TYPE

-- | A note describing the impact of the replacement.
data Note
    = IncreasesLaziness -- ^ The replacement is increases laziness, for example replacing @reverse (reverse x)@ with @x@ makes the code lazier.
    | DecreasesLaziness -- ^ The replacement is decreases laziness, for example replacing @(fst x, snd x)@ with @x@ makes the code stricter.
    | RemovesError String -- ^ The replacement removes errors, for example replacing @foldr1 (+)@ with @sum@ removes an error on @[]@, and might contain the text @\"on []\"@.
    | ValidInstance String String -- ^ The replacement assumes standard type class lemmas, a hint with the note @ValidInstance \"Eq\" \"x\"@ might only be valid if
                                  --   the @x@ variable has a reflexive @Eq@ instance.
    | Note String -- ^ An arbitrary note.
      deriving (Eq,Ord)

instance Show Note where
    show IncreasesLaziness = "increases laziness"
    show DecreasesLaziness = "decreases laziness"
    show (RemovesError x) = "removes error " ++ x
    show (ValidInstance x y) = "requires a valid `" ++ x ++ "` instance for `" ++ y ++ "`"
    show (Note x) = x


showNotes :: [Note] -> String
showNotes = intercalate ", " . map show . filter use
    where use ValidInstance{} = False -- Not important enough to tell an end user
          use _ = True

-- | How to classify an 'Idea'. If any matching field is @\"\"@ then it matches everything.
data Classify = Classify
    {classifySeverity :: Severity -- ^ Severity to set the 'Idea' to.
    ,classifyHint :: String -- ^ Match on 'Idea' field 'ideaHint'.
    ,classifyModule :: String -- ^ Match on 'Idea' field 'ideaModule'.
    ,classifyDecl :: String -- ^ Match on 'Idea' field 'ideaDecl'.
    }
    deriving Show

-- | A @LHS ==> RHS@ style hint rule.
data HintRule = HintRule
    {hintRuleSeverity :: Severity -- ^ Default severity for the hint.
    ,hintRuleName :: String -- ^ Name for the hint.
    ,hintRuleScope :: Scope -- ^ Module scope in which the hint operates.
    ,hintRuleLHS :: Exp_ -- ^ LHS
    ,hintRuleRHS :: Exp_ -- ^ RHS
    ,hintRuleSide :: Maybe Exp_ -- ^ Side condition, typically specified with @where _ = ...@.
    ,hintRuleNotes :: [Note] -- ^ Notes about application of the hint.
    }
    deriving Show

data RestrictType = RestrictModule | RestrictExtension | RestrictFlag | RestrictFunction deriving (Show,Eq,Ord)

data Restrict = Restrict
    {restrictType :: RestrictType
    ,restrictDefault :: Bool
    ,restrictName :: [String]
    ,restrictAs :: [String] -- for RestrictModule only, what you can import it as
    ,restrictWithin :: [(String, String)]
    } deriving Show

data Setting
    = SettingClassify Classify
    | SettingMatchExp HintRule
    | SettingRestrict Restrict
    | SettingArgument String -- ^ Extra command-line argument
    | Builtin String -- use a builtin hint set
    | Infix Fixity
      deriving Show
