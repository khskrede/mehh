





import System.Environment ( getArgs )

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity

main = do 
        (inFile:_) <- getArgs
        desc <- readPackageDescription normal inFile

        let packageDescription = flattenPackageDescription desc -- TODO: Can we do this?
            buildinfo = allBuildInfo packageDescription
            extensions = foldr (++) [] (map allExtensions buildinfo)





