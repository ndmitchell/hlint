
import Control.Exception.Extra
import System.Time.Extra
import System.Process.Extra


main :: IO ()
main = do
    retry 3 $ system_ "cabal install QuickCheck"
    system_ "hlint test --typecheck --quickcheck"
    (time,_) <- duration $ system "hlint src"
    putStrLn $ "Running HLint on self took " ++ showDuration time
    system_ "ghc -threaded -rtsopts -isrc -i. src/Paths.hs src/Main.hs --make -O -prof -auto-all -caf-all"
    system  "src/Main src +RTS -p"
    system_ "head -n32 Main.prof"
