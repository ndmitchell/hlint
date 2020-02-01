{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-
Find bindings within a let, and lists of statements
If you have n the same, error out

<TEST>
main = do a; a; a; a
main = do a; a; a; a; a; a -- ???
main = do a; a; a; a; a; a; a -- ???
main = do (do b; a; a; a); do (do c; a; a; a) -- ???
main = do a; a; a; b; a; a; a -- ???
main = do a; a; a; b; a; a
foo = a where {a = 1; b = 2; c = 3}; bar = a where {a = 1; b = 2; c = 3} -- ???
{-# ANN main "HLint: ignore Reduce duplication" #-}; main = do a; a; a; a; a; a -- @Ignore ???
{-# HLINT ignore main "Reduce duplication" #-}; main = do a; a; a; a; a; a -- @Ignore ???
{- HLINT ignore main "Reduce duplication" -}; main = do a; a; a; a; a; a -- @Ignore ???
</TEST>
-}


module Hint.Duplicate(duplicateHint) where

import Hint.Type (CrossHint, ModuleEx(..), Idea(..),rawIdeaN',Severity(Suggestion,Warning),showSrcLoc,ghcSrcLocToHSE)
import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Default
import Data.Maybe
import Data.Tuple.Extra
import Data.List hiding (find)
import qualified Data.Map as Map

import SrcLoc
import HsSyn
import Outputable
import Bag
import GHC.Util
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances

duplicateHint :: CrossHint
duplicateHint ms =
   -- Do expressions.
   dupes [ (m, d, y)
         | (m, d, x) <- ds
         , HsDo _ _ (LL _ y) :: HsExpr GhcPs <- universeBi x
         ] ++
  -- Bindings in a 'let' expression or a 'where' clause.
   dupes [ (m, d, y)
         | (m, d, x) <- ds
         , HsValBinds _ (ValBinds _ b _ ) :: HsLocalBinds GhcPs <- universeBi x
         , let y = bagToList b
         ]
    where
      ds = [(modName m, fromMaybe "" (declName d), unLoc d)
           | ModuleEx _ _ m _ <- map snd ms
           , d <- hsmodDecls (unLoc m)]

dupes :: (Outputable e, Data e) => [(String, String, [Located e])] -> [Idea]
dupes ys =
    [(rawIdeaN'
        (if length xs >= 5 then Hint.Type.Warning else Suggestion)
        "Reduce duplication" p1
        (unlines $ map unsafePrettyPrint xs)
        (Just $ "Combine with " ++
         showSrcLoc (ghcSrcLocToHSE (srcSpanStart p2))) []
     ){ideaModule = [m1, m2], ideaDecl = [d1, d2]}
    | ((m1, d1, SrcSpanD p1), (m2, d2, SrcSpanD p2), xs) <- duplicateOrdered 3 $ map f ys]
    where
      f (m, d, xs) =
        [((m, d, SrcSpanD (getLoc x)), extendInstances (stripLocs' x)) | x <- xs]

---------------------------------------------------------------------
-- DUPLICATE FINDING

-- | The position to return if we match at this point, and the map of where to go next
--   If two runs have the same vals, always use the first pos you find
data Dupe pos val = Dupe pos (Map.Map val (Dupe pos val))


find :: Ord val => [val] -> Dupe pos val -> (pos, Int)
find (v:vs) (Dupe p mp) | Just d <- Map.lookup v mp = second (+1) $ find vs d
find _ (Dupe p mp) = (p, 0)


add :: Ord val => pos -> [val] -> Dupe pos val -> Dupe pos val
add pos [] d = d
add pos (v:vs) (Dupe p mp) = Dupe p $ Map.insertWith f v (add pos vs $ Dupe pos Map.empty) mp
    where f new = add pos vs

duplicateOrdered :: forall pos val.
  (Ord pos, Default pos, Ord val) => Int -> [[(pos,val)]] -> [(pos,pos,[val])]
duplicateOrdered threshold xs = concat $ concat $ snd $ mapAccumL f (Dupe def Map.empty) xs
    where
        f :: Dupe pos val -> [(pos, val)] -> (Dupe pos val, [[(pos, pos, [val])]])
        f d xs = second overlaps $ mapAccumL (g pos) d $ takeWhile ((>= threshold) . length) $ tails xs
            where pos = Map.fromList $ zip (map fst xs) [0..]

        g :: Map.Map pos Int -> Dupe pos val -> [(pos, val)] -> (Dupe pos val, [(pos, pos, [val])])
        g pos d xs = (d2, res)
            where
                res = [(p,pme,take mx vs) | i >= threshold
                      ,let mx = maybe i (\x -> min i $ (pos Map.! pme) - x) $ Map.lookup p pos
                      ,mx >= threshold]
                vs = map snd xs
                (p,i) = find vs d
                pme = fst $ head xs
                d2 = add pme vs d

        overlaps (x@((_,_,n):_):xs) = x : overlaps (drop (length n - 1) xs)
        overlaps (x:xs) = x : overlaps xs
        overlaps [] = []
