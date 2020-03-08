
module Grep(runGrep) where

import Hint.All
import Apply
import Config.Type
import HSE.All
import Control.Monad
import Data.List
import Util
import Idea

import qualified HsSyn as GHC
import qualified BasicTypes as GHC
import qualified Outputable
import qualified ErrUtils
import Lexer
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import SrcLoc as GHC hiding (mkSrcSpan)
import GHC.Util.DynFlags


runGrep :: String -> ParseFlags -> [FilePath] -> IO ()
runGrep patt flags files = do
    exp <- case parseExpGhcWithMode (hseFlags flags) patt of
        POk _ a -> pure a
        PFailed _ loc err -> exitMessage $
          let msg = Outputable.showSDoc baseDynFlags $
                ErrUtils.pprLocErrMsg (ErrUtils.mkPlainErrMsg baseDynFlags loc err)
          in "Failed to parse " ++ msg ++ ", when parsing:\n " ++ patt
    let ghcUnit = GHC.noLoc $ GHC.ExplicitTuple GHC.noExt [] GHC.Boxed
    let rule = hintRules [HintRule Suggestion "grep" [] mempty (extendInstances exp) (extendInstances ghcUnit) Nothing]
    forM_ files $ \file -> do
        res <- parseModuleEx flags file Nothing
        case res of
            Left (ParseError sl msg ctxt) ->
                print $ rawIdeaN Error (if "Parse error" `isPrefixOf` msg then msg else "Parse error: " ++ msg) sl ctxt Nothing []
            Right m ->
                forM_ (applyHints [] rule [m]) $ \i ->
                    print i{ideaHint="", ideaTo=Nothing}
