
import Control.Exception.Extra
import System.Time.Extra
import System.Process.Extra


main :: IO ()
main = do
    -- retry 3 $ system_ "cabal install QuickCheck"
    -- FIXME: Temporarily disabled due to GHC 7.10 issues
    system_ "hlint test +RTS -K1K" --typecheck --quickcheck
    (time,_) <- duration $ system_ "UNIPLATE_VERBOSE=2 hlint src --hint=misc/HLint_Hints.hs +RTS -K1K"
    putStrLn $ "Running HLint on self took " ++ showDuration time
