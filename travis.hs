
import Neil

main :: IO ()
main = do
    cmd "hlint test"
    (time,_) <- duration $ cmdOut "time hlint src"
    print $ "Running HLint on self took " ++ show time ++ "s"
    cmd "ghc -threaded -rtsopts -isrc -i. src/Paths.hs src/Main.hs --make -O -prof -auto-all -caf-all"
    cmdOut "src/Main src +RTS -p"
    cmd "cat src/Main.prof"
