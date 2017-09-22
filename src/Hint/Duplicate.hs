{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

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
</TEST>
-}


module Hint.Duplicate(duplicateHint) where

import Hint.Type
import Data.Tuple.Extra
import Data.List hiding (find)
import qualified Data.Map as Map


duplicateHint :: CrossHint
duplicateHint ms =
    dupes [y | Do _ y :: Exp S <- universeBi modu] ++
    dupes [y | BDecls l y :: Binds S <- universeBi modu]
    where modu = map snd ms


dupes ys =
    [rawIdeaN
        (if length xs >= 5 then Warning else Suggestion)
        "Reduce duplication" p1
        (unlines $ map (prettyPrint . fmap (const p1)) xs)
        (Just $ "Combine with " ++ showSrcLoc (getPointLoc p2)) []
    | (p1,p2,xs) <- duplicateOrdered 3 $ map (map (srcInfoSpan . ann &&& dropAnn)) ys]


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


duplicateOrdered :: Ord val => Int -> [[(SrcSpan,val)]] -> [(SrcSpan,SrcSpan,[val])]
duplicateOrdered threshold xs = concat $ concat $ snd $ mapAccumL f (Dupe def Map.empty) xs
    where
        f d xs = second overlaps $ mapAccumL (g pos) d $ takeWhile ((>= threshold) . length) $ tails xs
            where pos = Map.fromList $ zip (map fst xs) [0..]

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
