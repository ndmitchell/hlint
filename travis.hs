
import Neil

main :: IO ()
main = do
    retry 3 $ cmd "cabal install QuickCheck"
    cmd "hlint test --typecheck --quickcheck"
    (time,_) <- duration $ cmdCode "hlint src"
    putStrLn $ "Running HLint on self took " ++ show time ++ "s"
    cmd "ghc -threaded -rtsopts -isrc -i. src/Paths.hs src/Main.hs --make -O -prof -auto-all -caf-all"
    cmdCode "src/Main src +RTS -p"
    cmd "head -n32 Main.prof"
