
module Hint.Util where

import Language.Haskell.Exts


declName :: HsDecl -> String
declName (HsPatBind _ (HsPVar (HsIdent name)) _ _) = name
declName (HsFunBind (HsMatch _ (HsIdent name) _ _ _ : _)) = name
declName x = error $ "declName: " ++ show x


parseHsModule :: FilePath -> IO HsModule
parseHsModule file = do
    res <- parseFile file
    case res of
        ParseOk x -> return x
        _ -> error $ "Failed to parse: " ++ file


