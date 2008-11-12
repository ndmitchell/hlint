
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
        ParseFailed src msg -> do
            putStrLn $ "" ++ showSrcLoc src ++ ": Parse failure, " ++ msg
            return $ HsModule nullSrcLoc (Module "") Nothing [] []


nullSrcLoc :: SrcLoc
nullSrcLoc = SrcLoc "" 0 0


showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc file line col) = file ++ ":" ++ show line ++ ":" ++ show col ++ ":"

