

-- GHC API stuff

import GHC
import GHC.Paths ( libdir )
import UniqFM
import Unique
import HscTypes
import CoreSyn
import DynFlags
import Outputable
import Module

-- Cabal stuff

import Distribution.Simple
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity
import Distribution.Simple.Utils
import qualified Distribution.ModuleName as ModName
import qualified Language.Haskell.Extension as LHE

-- JSON

import Text.JSON
import Text.JSON.Pretty
import Text.JSON.Generic
import Text.PrettyPrint.HughesPJ

-- Other

import System.Environment ( getArgs )

-- Input file is cabal file

main = do 
        (inFile1:_) <- getArgs

        -- Parse cabal file
        desc <- readPackageDescription normal inFile1
        let 
            -- TODO: Can we flatten like this?
            packageDescription = flattenPackageDescription desc 

        -- Get DynFlags
        dflags <- runGhc (Just libdir) $ do 
            getSessionDynFlags

        -- Set GHC Mode
        let 
            dflags2 = dflags{ ghcMode = CompManager }

        -- _______________________________________
        -- Set package name and version from cabal file
        -- to DynFlags

        let
            -- Get package name and version
            pkgid = PD.package packageDescription
            pkgname = pkgNameToString $ pkgName pkgid where
                pkgNameToString (PackageName a) = a
            pkgversion = pkgVersion pkgid
            dflags3 = dflags2{thisPackage=stringToPackageId pkgname}

        putStrLn ("Compiling package: " ++ pkgname)
        putStrLn ""

        -- _______________________________________
        -- Add extension flags from cabal file to DynFlags

        let
            -- Get ghc flags from cabal file and convert to 
            -- ExtensionFlags for use with the GHC API
            buildinfo = PD.allBuildInfo packageDescription
            extensions = foldl (++) [] (map PD.allExtensions buildinfo)
            extflags = extsToExtFlags extensions
            dflags4 = foldl xopt_set dflags3 extflags



        -- _______________________________________
        -- Set dependencies from cabal file to DynFlags

        let
            -- Get package dependency names
            deps = PD.buildDepends packageDescription
            depnames = map f deps where
                f (Dependency (PackageName n) _) = n
            pkgflags = map HidePackage depnames
            dflags5 = dflags4--{packageFlags=pkgflags}



        -- _______________________________________
        -- Set include dir

        let
            incs = foldl (++) [] (map PD.includeDirs buildinfo)
            dflags6 = dflags5{ importPaths=incs }
                              --libraryPaths=["../../"] }

        putStrLn $ show incs
        putStrLn ""
        putStrLn ""

        -- _______________________________________
        -- Create list of Haskell module file names to be compiled

        let prel = ["./"] ++ (map ((++) "../") depnames)
            mods = PD.exposedModules $ getLib $ PD.library $ packageDescription
            paths = map ModName.toFilePath mods

        fsearch <- mapM (findModuleFile prel ["hs", "lhs", "hsc"]) mods
        let files = map getFilePath fsearch
            

        putStrLn $ show $ files --paths
        putStrLn ""
        putStrLn ""



        -- _______________________________________
        --
        -- Compile all Haskell modules

        mapM (compile dflags6) files



getFilePath :: (String, String) -> String
getFilePath (a, b) = a++b


-- _______________________________________________________
--
-- Convert cabal info into GHC API Types


getLib :: Maybe PD.Library -> PD.Library
getLib Nothing = error "NOTHING!"
getLib (Just a) = a


extsToExtFlags :: [Extension] -> [ExtensionFlag]
extsToExtFlags = (map extToExtFlag) . func
    where
        func :: [Extension] -> [KnownExtension]
        func [] = []
        func ((EnableExtension a):xs) = a:(func xs)
        func ((DisableExtension a):xs) = a:(func xs)
        func ((UnknownExtension a):xs) = error a


extToExtFlag :: LHE.KnownExtension -> ExtensionFlag
extToExtFlag LHE.OverlappingInstances = Opt_OverlappingInstances
extToExtFlag LHE.UndecidableInstances = Opt_UndecidableInstances
extToExtFlag LHE.IncoherentInstances = Opt_IncoherentInstances
extToExtFlag LHE.DoRec = Opt_DoRec
extToExtFlag LHE.RecursiveDo = Opt_RecursiveDo
extToExtFlag LHE.ParallelListComp = Opt_ParallelListComp
extToExtFlag LHE.MultiParamTypeClasses = Opt_MultiParamTypeClasses
extToExtFlag LHE.MonomorphismRestriction = Opt_MonomorphismRestriction
extToExtFlag LHE.FunctionalDependencies = Opt_FunctionalDependencies
extToExtFlag LHE.Rank2Types = Opt_Rank2Types
extToExtFlag LHE.RankNTypes = Opt_RankNTypes
extToExtFlag LHE.PolymorphicComponents = Opt_PolymorphicComponents
extToExtFlag LHE.ExistentialQuantification = Opt_ExistentialQuantification
extToExtFlag LHE.ScopedTypeVariables = Opt_ScopedTypeVariables
extToExtFlag LHE.PatternSignatures = Opt_DefaultSignatures -- error "Opt_PatternSignatures"
extToExtFlag LHE.ImplicitParams = Opt_ImplicitParams
extToExtFlag LHE.FlexibleContexts = Opt_FlexibleContexts
extToExtFlag LHE.FlexibleInstances = Opt_FlexibleInstances
extToExtFlag LHE.EmptyDataDecls = Opt_EmptyDataDecls
extToExtFlag LHE.CPP = Opt_Cpp
extToExtFlag LHE.KindSignatures = Opt_KindSignatures
extToExtFlag LHE.BangPatterns = Opt_BangPatterns
extToExtFlag LHE.TypeSynonymInstances = Opt_TypeSynonymInstances
extToExtFlag LHE.TemplateHaskell = Opt_TemplateHaskell
extToExtFlag LHE.ForeignFunctionInterface = Opt_ForeignFunctionInterface
extToExtFlag LHE.Arrows = Opt_Arrows
extToExtFlag LHE.Generics = error "Opt_Generics"
extToExtFlag LHE.ImplicitPrelude = Opt_ImplicitPrelude
extToExtFlag LHE.NamedFieldPuns = error "Opt_NamedFieldPuns"
extToExtFlag LHE.PatternGuards = Opt_PatternGuards
extToExtFlag LHE.GeneralizedNewtypeDeriving = Opt_GeneralizedNewtypeDeriving
extToExtFlag LHE.ExtensibleRecords = error "Opt_ExtensibleRecords"
extToExtFlag LHE.RestrictedTypeSynonyms = error "Opt_RestrictedTypeSynonyms"
extToExtFlag LHE.HereDocuments = error "Opt_HereDocuments"
extToExtFlag LHE.MagicHash = Opt_MagicHash
extToExtFlag LHE.TypeFamilies = Opt_TypeFamilies
extToExtFlag LHE.StandaloneDeriving = Opt_StandaloneDeriving
extToExtFlag LHE.UnicodeSyntax = Opt_UnicodeSyntax
extToExtFlag LHE.UnliftedFFITypes = Opt_UnliftedFFITypes
extToExtFlag LHE.LiberalTypeSynonyms = Opt_LiberalTypeSynonyms
extToExtFlag LHE.TypeOperators = Opt_TypeOperators
extToExtFlag LHE.RecordWildCards = Opt_RecordWildCards
extToExtFlag LHE.RecordPuns = Opt_RecordPuns
extToExtFlag LHE.DisambiguateRecordFields = Opt_DisambiguateRecordFields
extToExtFlag LHE.OverloadedStrings = Opt_OverloadedStrings
extToExtFlag LHE.GADTs = Opt_GADTs
extToExtFlag LHE.MonoPatBinds = Opt_MonoPatBinds
extToExtFlag LHE.RelaxedPolyRec = Opt_RelaxedPolyRec
extToExtFlag LHE.ExtendedDefaultRules = Opt_ExtendedDefaultRules
extToExtFlag LHE.UnboxedTuples = Opt_UnboxedTuples
extToExtFlag LHE.DeriveDataTypeable = Opt_DeriveDataTypeable
extToExtFlag LHE.ConstrainedClassMethods = Opt_ConstrainedClassMethods
extToExtFlag LHE.PackageImports = Opt_PackageImports
extToExtFlag LHE.ImpredicativeTypes = Opt_ImpredicativeTypes
extToExtFlag LHE.NewQualifiedOperators = error "Opt_NewQualifiedOperators"
extToExtFlag LHE.PostfixOperators = Opt_PostfixOperators
extToExtFlag LHE.QuasiQuotes = Opt_QuasiQuotes
extToExtFlag LHE.TransformListComp = Opt_TransformListComp
extToExtFlag LHE.ViewPatterns = Opt_ViewPatterns
extToExtFlag LHE.XmlSyntax = error "Opt_XmlSyntax"
extToExtFlag LHE.RegularPatterns = error "Opt_RegularPatterns"
extToExtFlag LHE.TupleSections = Opt_TupleSections
extToExtFlag LHE.GHCForeignImportPrim = Opt_GHCForeignImportPrim
extToExtFlag LHE.NPlusKPatterns = Opt_NPlusKPatterns
extToExtFlag LHE.DoAndIfThenElse = Opt_DoAndIfThenElse
extToExtFlag LHE.RebindableSyntax = Opt_RebindableSyntax
extToExtFlag LHE.ExplicitForAll = Opt_ExplicitForAll
extToExtFlag LHE.DatatypeContexts = Opt_DatatypeContexts
extToExtFlag LHE.MonoLocalBinds = Opt_MonoLocalBinds
extToExtFlag LHE.DeriveFunctor = Opt_DeriveFunctor
extToExtFlag LHE.DeriveTraversable = Opt_DeriveTraversable
extToExtFlag LHE.DeriveFoldable = Opt_DeriveFoldable





-- _______________________________________________________
--
-- Compile to Core and Generate JSCore for single Haskell file


compile dflags inFile = do 
    core <- runGhc (Just libdir) $ do
        setSessionDynFlags dflags

        core <- compileToCoreSimplified inFile
        return core

    putStrLn $ show $ pp_value $ coreModToJS core
    putStrLn "bleh"

coreModToJS :: CoreModule -> JSValue
coreModToJS (CoreModule name types binds) = 
    JSObject $ toJSObject $
    [( "%module", toJSON $ showSDoc $ ppr name )]--,
--     ( "tdefg", typesToJS types),
--     ( "binds", bindsToJS binds)]


typesToJS :: TypeEnv -> JSValue
typesToJS p = JSObject $ toJSObject $ map func $ ufmToList p
    where
        func :: (Unique, TyThing) -> (String, JSValue)
        -- TODO: Generate proper JSON values
        func (a, b) = ( showSDoc $ ppr a, toJSON $ showSDoc $ ppr b)


bindsToJS :: CoreProgram -> JSValue
bindsToJS progs = JSObject $ toJSObject $ map f $ progs
    where
        f :: CoreBind -> (String, JSValue)
        f (NonRec b expr) = ("NonRec" , corebindToJS (b, expr))
        f (Rec list) = ("Rec", JSArray $ map corebindToJS list)


corebindToJS :: (CoreBndr, (Expr CoreBndr)) -> JSValue
corebindToJS (a, b) = JSObject $ toJSObject $ [(show a, exprToJS b)]




-- BINDERS




-- EXPRESSIONS


exprToJS :: Expr b -> JSValue
exprToJS (Var a) = toJSON a 
exprToJS (Lit a) = toJSON "Lit"
exprToJS (App a b) = toJSON "App"
exprToJS (Lam a b) = JSArray [toJSON "Lam", exprToJS b]
exprToJS (Let a b) = toJSON "Let"
exprToJS (Case a b c d) = toJSON "Case"
exprToJS (Cast a b) = toJSON "Cast"
exprToJS (Tick a b) = toJSON "Tick"
exprToJS (Type a) = toJSON "Type"
exprToJS (Coercion a) = toJSON "Coercion"

