{-
    Find and match:

    1:2:[] -> [1,2]
    ['a':'b'] -> "ab"
    [a]++b -> a : b, but only if not in a chain of ++'s
    "c"++x -> 'c':x, but only if not in a chain of ++'s
-}


module Hint.List where

import Hint.Util
import Hint.Type
import Language.Haskell.Exts


listHint :: Hint
listHint = listDecl

listDecl :: HsDecl -> [Idea]
listDecl = concatMap listExp . children0Exp nullSrcLoc

listExp :: (SrcLoc,HsExp) -> [Idea]
listExp (loc,x) = case x of
    HsList xs | not (null xs) && all isCharExp xs ->
        [Idea "Use a string literal" loc (Just $ prettyPrint x) (Just $ prettyPrint $ HsLit $ HsString [x | HsLit (HsChar x) <- xs])]
    _ -> concatMap listExp $ children1Exp loc x

