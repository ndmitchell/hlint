-- Adapted from https://github.com/mpickering/apply-refact.git.

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module GHC.Util.Refact.Fixity (applyFixities) where

import BasicTypes (Fixity(..), defaultFixity, compareFixity, negateFixity, FixityDirection(..), SourceText(..))
import HsSyn
import RdrName
import OccName
import SrcLoc
import Data.Maybe
import Data.Data hiding (Fixity)
import Data.Generics.Uniplate.Data

-- | Rearrange infix expressions to account for fixity.
applyFixities :: Data a => [(String, Fixity)] -> a -> a
applyFixities fixities m =
  let m' = transformBi (expFix fixities) m
      m'' = transformBi (patFix fixities) m'
  in m''

getFixities fixities =
  if null fixities then baseFixities else fixities

expFix :: [(String, Fixity)] -> LHsExpr GhcPs -> LHsExpr GhcPs
expFix fixities (L loc (OpApp _ l op r)) =
  mkOpAppRn (getFixities fixities) loc l op (findFixity (getFixities fixities) op) r

expFix _ e = e

patFix :: [(String, Fixity)] -> LPat GhcPs -> LPat GhcPs
patFix fixities (dL -> L _ (ConPatIn op (InfixCon pat1 pat2))) =
  mkConOpPatRn (getFixities fixities) op (findFixity' (getFixities fixities) op) pat1 pat2

patFix _ p = p

getIdent :: LHsExpr GhcPs -> String
getIdent (unLoc -> HsVar _ (L _ n)) = occNameString . rdrNameOcc $ n
getIdent _ = error "Must be HsVar"

---------------------------
-- Modified from GHC Renamer

mkConOpPatRn ::
             [(String, Fixity)]
          -> Located RdrName -> Fixity          -- Operator and fixity
          -> LPat GhcPs
          -> LPat GhcPs
          -> LPat GhcPs
mkConOpPatRn fs op2 fix2 p1@(dL->L loc (ConPatIn op1 (InfixCon p11 p12))) p2
  | nofix_error =
    ConPatIn op2 (InfixCon p1 p2)

 | associate_right =
   let new_p = mkConOpPatRn fs op2 fix2 p12 p2 in ConPatIn op1 (InfixCon p11 (cL loc new_p))

 | otherwise =
     ConPatIn op2 (InfixCon p1 p2)

  where
    fix1 = findFixity' fs op1
    (nofix_error, associate_right) = compareFixity fix1 fix2

mkConOpPatRn _ op _ p1 p2                         -- Default case, no rearrangment
  = ConPatIn op (InfixCon p1 p2)

mkOpAppRn ::
             [(String, Fixity)]
          -> SrcSpan
          -> LHsExpr GhcPs              -- Left operand; already rearrange
          -> LHsExpr GhcPs -> Fixity            -- Operator and fixity
          -> LHsExpr GhcPs                      -- Right operand (not an OpApp, but might
                                                -- be a NegApp)
          -> LHsExpr GhcPs

-- (e11 `op1` e12) `op2` e2
mkOpAppRn fs loc e1@(L _ (OpApp x1 e11 op1 e12)) op2 fix2 e2
  | nofix_error
  = L loc (OpApp noExt e1 op2 e2)

  | associate_right =
    let new_e = mkOpAppRn fs loc' e12 op2 fix2 e2 in L loc (OpApp x1 e11 op1 new_e)
  where
    loc'= combineLocs e12 e2
    fix1 = findFixity fs op1
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--      (- neg_arg) `op` e2
mkOpAppRn fs loc e1@(L _ (NegApp _ neg_arg neg_name)) op2 fix2 e2
  | nofix_error
  = L loc (OpApp noExt e1 op2 e2)

  | associate_right
  = let new_e = mkOpAppRn fs loc' neg_arg op2 fix2 e2
        res = L loc (NegApp noExt new_e neg_name) in
    res

  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--      e1 `op` - neg_arg
mkOpAppRn _ loc e1 op1 fix1 e2@(L _ NegApp {})     -- NegApp can occur on the right
  | not associate_right                 -- We *want* right association
  = L loc (OpApp noExt e1 op1 e2)
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--      Default case
mkOpAppRn _ loc e1 op _fix e2                  -- Default case, no rearrangment
  = L loc (OpApp noExt e1 op e2)

findFixity :: [(String, Fixity)] -> LHsExpr GhcPs -> Fixity
findFixity fs r = askFix fs (getIdent r)

findFixity' :: [(String, Fixity)] -> Located RdrName -> Fixity
findFixity' fs r = askFix fs (occNameString . rdrNameOcc . unLoc $ r)

askFix :: [(String, Fixity)] -> String -> Fixity
askFix xs = \k -> lookupWithDefault defaultFixity k xs
    where
        lookupWithDefault def_v k mp1 = fromMaybe def_v $ lookup k mp1

-- | All fixities defined in the Prelude.
preludeFixities :: [(String, Fixity)]
preludeFixities = concat
    [infixr_ 9  ["."]
    ,infixl_ 9  ["!!"]
    ,infixr_ 8  ["^","^^","**"]
    ,infixl_ 7  ["*","/","quot","rem","div","mod",":%","%"]
    ,infixl_ 6  ["+","-"]
    ,infixr_ 5  [":","++"]
    ,infix_  4  ["==","/=","<","<=",">=",">","elem","notElem"]
    ,infixr_ 3  ["&&"]
    ,infixr_ 2  ["||"]
    ,infixl_ 1  [">>",">>="]
    ,infixr_ 1  ["=<<"]
    ,infixr_ 0  ["$","$!","seq"]
    ]

-- | All fixities defined in the base package.
--
--   Note that the @+++@ operator appears in both Control.Arrows and
--   Text.ParserCombinators.ReadP. The listed precedence for @+++@ in
--   this list is that of Control.Arrows.
baseFixities :: [(String, Fixity)]
baseFixities = preludeFixities ++ concat
    [infixl_ 9 ["!","//","!:"]
    ,infixl_ 8 ["shift","rotate","shiftL","shiftR","rotateL","rotateR"]
    ,infixl_ 7 [".&."]
    ,infixl_ 6 ["xor"]
    ,infix_  6 [":+"]
    ,infixl_ 5 [".|."]
    ,infixr_ 5 ["+:+","<++","<+>"] -- fixity conflict for +++ between ReadP and Arrow
    ,infix_  5 ["\\\\"]
    ,infixl_ 4 ["<$>","<$","<*>","<*","*>","<**>"]
    ,infix_  4 ["elemP","notElemP"]
    ,infixl_ 3 ["<|>"]
    ,infixr_ 3 ["&&&","***"]
    ,infixr_ 2 ["+++","|||"]
    ,infixr_ 1 ["<=<",">=>",">>>","<<<","^<<","<<^","^>>",">>^"]
    ,infixl_ 0 ["on"]
    ,infixr_ 0 ["par","pseq"]
    ]

infixr_, infixl_, infix_ :: Int -> [String] -> [(String,Fixity)]
infixr_ = fixity InfixR
infixl_ = fixity InfixL
infix_  = fixity InfixN

-- Internal: help function for the above definitions.
fixity :: FixityDirection -> Int -> [String] -> [(String, Fixity)]
fixity a p = map (,Fixity (SourceText "") p a)
