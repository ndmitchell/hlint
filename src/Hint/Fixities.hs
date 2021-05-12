{-

Raise a warning if you have redundant brackets in nested infix expressions.

<TEST>
yes = 1 + (2 * 3) -- @Ignore 1 + 2 * 3
yes = (2 * 3) + 1 -- @Ignore 2 * 3 + 1
no = (1 + 2) * 3
no = 3 * (1 + 2)
no = 1 + 2 * 3
no = 2 * 3 + 1
yes = (a >>= f) >>= g -- @Ignore a >>= f >>= g
no = (a >>= \x -> b) >>= g
</TEST>
-}

module Hint.Fixities(fixitiesHint) where

import Hint.Type(DeclHint,Idea(..),rawIdea,toSSA)
import Config.Type
import Control.Monad
import Data.List.Extra
import Data.Map
import Data.Generics.Uniplate.DataOnly
import Refact.Types

import GHC.Types.Fixity(compareFixity)
import Fixity
import GHC.Hs
import GHC.Util
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence

fixitiesHint :: [Setting] -> DeclHint
fixitiesHint settings _ _ x =
  concatMap (infixBracket fixities) (childrenBi x :: [LHsExpr GhcPs])
   where
     fixities = foldMap getFixity settings `mappend` fromList (toFixity <$> defaultFixities)
     getFixity (Infix x) = uncurry Data.Map.singleton (toFixity x)
     getFixity _ = mempty

infixBracket :: Map String Fixity -> LHsExpr GhcPs -> [Idea]
infixBracket fixities = f Nothing
  where
    msg = "Redundant bracket due to operator fixities"
    f p o = cur p o <> concat [f (Just (i, o, gen)) x | (i, (x, gen)) <- zipFrom 0 $ holes o]
    cur p v = do
      Just (i, o, gen) <- [p]
      Just x <- [remParen v]
      guard $ redundantInfixBracket fixities i o x
      pure $
        rawIdea Ignore msg (locA (getLoc v)) (unsafePrettyPrint o)
        (Just (unsafePrettyPrint (gen x))) [] [Replace (findType v) (toSSA v) [("x", toSSA x)] "x"]

redundantInfixBracket :: Map String Fixity -> Int -> LHsExpr GhcPs -> LHsExpr GhcPs -> Bool
redundantInfixBracket fixities i parent child
    | L _ (OpApp _ _ (L _ (HsVar _ (L _ (Unqual p)))) _) <- parent
    , L _ (OpApp _ _ (L _ (HsVar _ (L _ (Unqual c)))) (L _ cr)) <- child =
    let (lop, rop)
            | i == 0 = (c, p)
            | otherwise = (p, c)
    in
    case compareFixity <$> (fixities Data.Map.!? occNameString lop) <*> (fixities Data.Map.!? occNameString rop) of
    Just (False, r)
        | i == 0 -> not (needParenAsChild cr || r)
        | otherwise -> r
    _ -> False
    | otherwise = False

needParenAsChild :: HsExpr p -> Bool
needParenAsChild HsLet{} = True
needParenAsChild HsDo{} = True
needParenAsChild HsLam{} = True
needParenAsChild HsLamCase{} = True
needParenAsChild HsCase{} = True
needParenAsChild HsIf{} = True
needParenAsChild _ = False
