{-

Raise a warning if negation precedence may appear ambiguous to human readers.

<TEST>
yes = -1 ^ 2 -- @Suggestion -(1 ^ 2)
yes = -x ^ y -- @Suggestion -(x ^ y)
yes = -5 `plus` 3 -- @Suggestion -(5 `plus` 3)
yes = -f x `mod` y -- @Suggestion -(f x `mod` y)
yes = -x `mod` y -- @Suggestion -(x `mod` y)
no = -(5 + 3)
no = -5 + 3
no = -(f x)
no = -x
</TEST>
-}

module Hint.Negation(negationParensHint) where

import Hint.Type(DeclHint,Idea(..),rawIdea,toSSA)
import Config.Type
import Data.Generics.Uniplate.DataOnly
import Refact.Types
import GHC.Hs
import GHC.Util
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import GHC.Types.SrcLoc

-- | See [motivating issue #1484](https://github.com/ndmitchell/hlint/issues/1484).
--
-- == Implementation note
--
-- The original intention was to compare fixities so as
-- to only fire the rule when the operand of prefix negation
-- has higher fixity than the negation itself (fixity 6).
--
-- However, since there do not exist any numerically-valued
-- operators with lower fixity than 6
-- (see [table](https://www.haskell.org/onlinereport/decls.html#sect4.4.2)),
-- we do not have to worry about fixity comparisons.
negationParensHint :: DeclHint
negationParensHint _ _ x =
  concatMap negatedOp (universeBi x :: [LHsExpr GhcPs])

negatedOp :: LHsExpr GhcPs -> [Idea]
negatedOp e =
  case e of
    L b1 (NegApp a1 inner@(L _ OpApp {}) a2) ->
      pure $
        rawIdea
          Suggestion
          "Parenthesize unary negation"
          (locA (getLoc e))
          (unsafePrettyPrint e)
          (Just renderedNewExpr)
          []
          [Replace (findType e) (toSSA e) [] renderedNewExpr]
        where
          renderedNewExpr = unsafePrettyPrint newExpr
          parenthesizedOperand = addParen inner
          newExpr = L b1 $ NegApp a1 parenthesizedOperand a2
    _ -> []
