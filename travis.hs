
import System.Time.Extra
import System.Process.Extra


main :: IO ()
main = do
    system_ "hlint --generate-summary"
    system_ "hlint --test +RTS -K512K"
    (time,_) <- duration $ system_ "hlint src" -- "UNIPLATE_VERBOSE=-1 hlint src +RTS -K1K"
    putStrLn $ "Running HLint on self took " ++ showDuration time
