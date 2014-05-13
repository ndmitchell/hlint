
import Neil

main :: IO ()
main = do
    cmd "hlint test --typecheck"
    (time,_) <- duration $ cmdCode "hlint src"
    putStrLn $ "Running HLint on self took " ++ show time ++ "s"
    cmd "ghc -threaded -rtsopts -isrc -i. src/Paths.hs src/Main.hs --make -O -prof -auto-all -caf-all"
    cmdCode "src/Main src +RTS -p"
    cmd "head -n32 Main.prof"
