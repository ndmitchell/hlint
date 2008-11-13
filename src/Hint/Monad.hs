{-# LANGUAGE ViewPatterns #-}

{-
    Find and match:

    mapM, foldM, forM, replicateM, sequence, zipWithM
    not at the last line of a do statement, or to the left of >>
-}


module Hint.Monad where

import Control.Monad
import Hint.Util
import Hint.Type
import Language.Haskell.Exts


badFuncs = ["mapM","foldM","forM","replicateM","sequence","zipWithM"]


monadHint :: Hint
monadHint = concatMap monadExp . universeExp nullSrcLoc

monadExp :: (SrcLoc,HsExp) -> [Idea]
monadExp (loc,x) = case x of
        (view -> App2 op x1 x2) | op ~= ">>" -> f x1
        HsDo xs -> concat [f x | HsQualifier x <- init xs]
        HsMDo xs -> monadExp (loc, HsDo xs)
        _ -> []
    where
        f x = [Idea "Use a more efficient monadic variant" loc (Just $ prettyPrint x) (Just $ prettyPrint y)
              |Just y <- [monadCall x]]


-- see through HsParen and down if/case etc
monadCall :: HsExp -> Maybe HsExp
monadCall (HsParen x) = liftM HsParen $ monadCall x
monadCall (HsApp x y) = liftM (`HsApp` y) $ monadCall x
monadCall x | x:_ <- filter (x ~=) badFuncs = Just $ toVar (x ++ "_")
monadCall _ = Nothing
