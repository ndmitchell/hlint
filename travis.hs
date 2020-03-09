
import System.Time.Extra
import System.Process.Extra


main :: IO ()
main = do
    -- retry 3 $ system_ "cabal install QuickCheck"
    -- FIXME: Temporarily disabled --typecheck --quickcheck due to GHC 7.10 issues
    system_ "hlint test +RTS -K512K" --typecheck --quickcheck
    (time,_) <- duration $ system_ "hlint src --with-group=extra --with-group=future" -- "UNIPLATE_VERBOSE=-1 hlint src +RTS -K1K"
    putStrLn $ "Running HLint on self took " ++ showDuration time
