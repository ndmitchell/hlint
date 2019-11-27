
module Config.Type(
    Severity(..), Classify(..), HintRule(..), Note(..), Setting(..),
    Restrict(..), RestrictType(..), SmellType(..),
    defaultHintName, isUnifyVar, showNotes, getSeverity, getRestrictType, getSmellType
    ) where

import HSE.All
import Data.Char
import Data.List.Extra
import Prelude

import qualified HsSyn
import GHC.Util

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
isUnifyVar [] = False
isUnifyVar xs = all (== '?') xs


---------------------------------------------------------------------
-- TYPE

-- | A note describing the impact of the replacement.
data Note
    = IncreasesLaziness -- ^ The replacement is increases laziness, for example replacing @reverse (reverse x)@ with @x@ makes the code lazier.
    | DecreasesLaziness -- ^ The replacement is decreases laziness, for example replacing @(fst x, snd x)@ with @x@ makes the code stricter.
    | RemovesError String -- ^ The replacement removes errors, for example replacing @foldr1 (+)@ with @sum@ removes an error on @[]@, and might contain the text @\"on []\"@.
    | ValidInstance String String -- ^ The replacement assumes standard type class lemmas, a hint with the note @ValidInstance \"Eq\" \"x\"@ might only be valid if
                                  --   the @x@ variable has a reflexive @Eq@ instance.
    | RequiresExtension String -- ^ The replacement requires this extension to be available.
    | Note String -- ^ An arbitrary note.
      deriving (Eq,Ord)

instance Show Note where
    show IncreasesLaziness = "increases laziness"
    show DecreasesLaziness = "decreases laziness"
    show (RemovesError x) = "removes error " ++ x
    show (ValidInstance x y) = "requires a valid `" ++ x ++ "` instance for `" ++ y ++ "`"
    show (RequiresExtension x) = "may require `{-# LANGUAGE " ++ x ++ " #-}` adding to the top of the file"
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
    ,hintRuleLHS :: Exp SrcSpanInfo -- ^ LHS
    ,hintRuleRHS :: Exp SrcSpanInfo -- ^ RHS
    ,hintRuleSide :: Maybe (Exp SrcSpanInfo) -- ^ Side condition, typically specified with @where _ = ...@.
    ,hintRuleNotes :: [Note] -- ^ Notes about application of the hint.
    -- We wrap these GHC elements in 'W' in order that we may derive 'Show'.
    ,hintRuleGhcScope :: W Scope' -- ^ Module scope in which the hint operates (GHC parse tree).
    ,hintRuleGhcLHS :: W (HsSyn.LHsExpr HsSyn.GhcPs) -- ^ LHS (GHC parse tree).
    ,hintRuleGhcRHS :: W (HsSyn.LHsExpr HsSyn.GhcPs) -- ^ RHS (GHC parse tree).
    ,hintRuleGhcSide :: Maybe (W (HsSyn.LHsExpr HsSyn.GhcPs))  -- ^ Side condition (GHC parse tree).
    }
    deriving Show

data RestrictType = RestrictModule | RestrictExtension | RestrictFlag | RestrictFunction deriving (Show,Eq,Ord)

data Restrict = Restrict
    {restrictType :: RestrictType
    ,restrictDefault :: Bool
    ,restrictName :: [String]
    ,restrictAs :: [String] -- for RestrictModule only, what module names you can import it as
    ,restrictWithin :: [(String, String)]
    ,restrictMessage :: Maybe String
    } deriving Show

data SmellType = SmellLongFunctions | SmellLongTypeLists | SmellManyArgFunctions | SmellManyImports
  deriving (Show,Eq,Ord)

getSmellType :: String -> Maybe SmellType
getSmellType "long functions" = Just SmellLongFunctions
getSmellType "long type lists" = Just SmellLongTypeLists
getSmellType "many arg functions" = Just SmellManyArgFunctions
getSmellType "many imports" = Just SmellManyImports
getSmellType _ = Nothing

data Setting
    = SettingClassify Classify
    | SettingMatchExp HintRule
    | SettingRestrict Restrict
    | SettingArgument String -- ^ Extra command-line argument
    | SettingSmell SmellType Int
    | Builtin String -- use a builtin hint set
    | Infix Fixity
      deriving Show
