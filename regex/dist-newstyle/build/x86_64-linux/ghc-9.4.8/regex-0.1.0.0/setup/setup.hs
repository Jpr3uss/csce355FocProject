import Distribution.Simple
import System.Directory (copyFile)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { postBuild = \_ _ _ _ -> do
        let buildDir = "dist-newstyle/build/x86_64-linux/ghc-9.4.8/regex-0.1.0.0/build/regex"
        let outputBinary = buildDir </> "regex"
        let destination = "out/regex"
        copyFile outputBinary destination
        putStrLn $ "Copied binary to " ++ destination
    }