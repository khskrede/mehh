
import GHC
import Outputable
import GHC.Paths (libdir)
import DynFlags
import System (getArgs)


main = defaultErrorHandler defaultDynFlags $ do
    (inFile:outFile:_) <- getArgs

    c <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags
        compileToCoreSimplified inFile

    writeFile outFile (showSDoc (ppr c))
