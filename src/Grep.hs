
module Grep(runGrep) where

import Hint.All
import Apply
import Config.Type
import GHC.All
import Control.Monad
import Data.List
import Util
import Idea

import qualified GHC.Hs as GHC
import qualified BasicTypes as GHC
import qualified Outputable
import qualified ErrUtils
import Lexer
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import SrcLoc as GHC hiding (mkSrcSpan)
import GHC.Util.DynFlags
import Bag

runGrep :: String -> ParseFlags -> [FilePath] -> IO ()
runGrep patt flags files = do
    exp <- case parseExpGhcWithMode flags patt of
        POk _ a -> pure a
        PFailed ps -> exitMessage $
          let (_, errs) = getMessages ps baseDynFlags
              errMsg = head (bagToList errs)
              msg = Outputable.showSDoc baseDynFlags $ ErrUtils.pprLocErrMsg errMsg
          in "Failed to parse " ++ msg ++ ", when parsing:\n " ++ patt
    let ghcUnit = GHC.noLoc $ GHC.ExplicitTuple GHC.noExtField [] GHC.Boxed
    let rule = hintRules [HintRule Suggestion "grep" [] mempty (extendInstances exp) (extendInstances ghcUnit) Nothing]
    forM_ files $ \file -> do
        res <- parseModuleEx flags file Nothing
        case res of
            Left (ParseError sl msg ctxt) ->
                print $ rawIdeaN Error (if "Parse error" `isPrefixOf` msg then msg else "Parse error: " ++ msg) sl ctxt Nothing []
            Right m ->
                forM_ (applyHints [] rule [m]) $ \i ->
                    print i{ideaHint="", ideaTo=Nothing}
