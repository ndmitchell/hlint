
import Control.Exception.Extra
import System.Time.Extra
import System.Process.Extra


main :: IO ()
main = do
    -- retry 3 $ system_ "cabal install QuickCheck"
    -- FIXME: Temporarily disabled --typecheck --quickcheck due to GHC 7.10 issues
    -- FIXME: reads 1M in two places rather than 1K bceause of space leak in yaml-0.10.0
    --        https://github.com/ndmitchell/hlint/issues/519
    system_ "hlint test +RTS -K1M" --typecheck --quickcheck
    (time,_) <- duration $ system_ "UNIPLATE_VERBOSE=-1 hlint src +RTS -K1M"
    putStrLn $ "Running HLint on self took " ++ showDuration time
