
import GHC
import Outputable
import GHC.Paths                    ( libdir )
import DynFlags
import System                       ( getArgs )

-- CoreModule
import HscTypes                     ( CoreModule(..), TypeEnv )
import qualified CoreSyn as Hs
import Var
import Name
import SimplCore
import CoreUtils

import Literal
import FastString
import Data.Char

import Module                       ( ModuleName, moduleNameString, packageIdString )

import Text.JSON
import Text.JSON.Pretty
import Text.JSON.Generic

-- Main compiler functions
main = do (inFile:outFile:_) <- getArgs
          compile inFile outFile


compile :: FilePath -> FilePath -> IO ()
compile inFile outFile = 
     defaultErrorHandler defaultDynFlags $ do
        core <- getCore inFile
        let progString = getString core
        writeFile outFile progString
        --writeFile outFile (showSDoc (ppr core))

        putStrLn $ show $ pp_value $ getJSCore core

getCore :: FilePath -> IO CoreModule
getCore path = runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags dflags
                -- parse, typecheck, desugar, simplify
                compileToCoreSimplified path 


getJSCore :: CoreModule -> JSValue
getJSCore (CoreModule name typeenv binds imports) = 
    JSString $ toJSString "whaat"


-- Traverse CoreModule and generate output
getString :: CoreModule -> String
getString (CoreModule name typeenv binds imports) = 
    "Module(\n\n"
    ++ (getNameString name) ++ ", \n\n" 
    ++ (getImportsString imports) ++ ", \n\n"
    ++ (getTypeEnvString typeenv) ++ ", \n\n"
    ++ (getBindsString binds) ++ ", \n\n"
    ++ ") "


-- Generate string from module name
getNameString :: Module -> String
getNameString theModule = 
    "ModuleName( " ++ 
    (moduleNameString $ moduleName theModule) ++ ", " ++ 
    (packageIdString $ modulePackageId theModule) ++ " )"


-- Generate string from imports
getImportsString :: [Module] -> String
getImportsString imports = 
    "Imports( " ++ (getImportsString' imports) ++ " )"

getImportsString' :: [Module] -> String
getImportsString' []        = []
getImportsString' (x:xs)    = (getNameString x) ++ " " ++ getImportsString' xs


-- Generate string from type env
getTypeEnvString :: TypeEnv -> String
getTypeEnvString typeenv = 
    "TypeEnv( " ++ (getTypeEnvString' typeenv) ++ " )"

getTypeEnvString' :: TypeEnv -> String
getTypeEnvString' typeenv = 
    "mehblehbleh"


-- Generate string from binds
getBindsString :: [Hs.CoreBind] -> String
getBindsString binds = 
    "Binds( " ++ (getBindsString' binds) ++ " )"

getBindsString' :: [Hs.CoreBind] -> String
getBindsString' []      = []
getBindsString' (x:xs)  = getBindString x ++ ", " ++ getBindsString' xs

getBindString :: Hs.CoreBind -> String
getBindString (Hs.NonRec name expr)     = getNonRecString (Hs.NonRec name expr) 
getBindString (Hs.Rec [(name,expr)])    = getRecString (Hs.Rec [(name, expr)])
getBindString (Hs.Rec _)                = error "getBindString"

getNonRecString :: Hs.CoreBind -> String
getNonRecString (Hs.NonRec name expr) = getExprString expr

getRecString :: Hs.CoreBind -> String
getRecString (Hs.Rec [(name,expr)]) = getExprString expr


-- Generate string from expression
getExprString :: Hs.CoreExpr -> String
getExprString (Hs.App f arg) = 
    "App( " ++ (getExprString f) ++ "," ++ (getExprString arg) ++  " )"
getExprString (Hs.Lam var rhs) = 
    "Lam()"
getExprString (Hs.Case expr var typ alts) = 
    "Case()"
getExprString (Hs.Let _ _) = 
    "Let()"
getExprString (Hs.Cast _ _) = 
    "Cast()"
getExprString (Hs.Note _ _) = 
    "Note()"
getExprString (Hs.Type _) =
    "Type"
getExprString (Hs.Var var) =
    "Var( " ++ (getOccString var) ++ " )"
getExprString (Hs.Lit lit) =
    "Lit( " ++ (getLitString lit) ++ " )"

getLitString :: Literal -> String
getLitString (MachChar char) = 
    "Char( \'" ++ char:"\' )"
getLitString (MachStr str) = 
    "Str( \"" ++ unpackFS str ++ "\" )"
getLitString (MachNullAddr) = 
    "NullAddr()"
getLitString (MachInt int) = 
    "Int( " ++ (show int) ++ " )"
getLitString (MachInt64 int) = 
    "Int64( " ++ (show int) ++ " )"
getLitString (MachWord word) = 
    "Word( " ++ (show word) ++ " )"
getLitString (MachWord64 word) = 
    "Word64( " ++ (show word) ++ " )"
getLitString (MachFloat float) = 
    "Float( " ++ (show float) ++ " )"
getLitString (MachDouble double) = 
    "Double( " ++ (show double) ++ " )"
getLitString (MachLabel meh meh2 meh3) = 
    "Label()"

--simType :: CoreModule -> CoreModule
--simType (CoreModule name typeenv binds imports) =
--    CoreModule name typeenv (map simType' binds) imports

--simType' :: Hs.CoreBind -> Hs.CoreBind
--simType' (Hs.NonRec name expr)      = Hs.NonRec name (simplifyExpr expr)
--simType' (Hs.Rec [(name, expr)])    = Hs.Rec [(name, simplifyExpr expr)]
--simType' (Hs.Rec _)                 = error "simType': more than one binding in Rec"

--
-- The function unType removes all type information from the function
-- definitions of a CoreModule
--

-- unType :: CoreModule -> CoreModule
-- unType (CoreModule name typeenv binds imports) = 
--    CoreModule name typeenv (map unTypeBind binds) imports

-- Remove type information from binders

--unTypeBind :: Hs.CoreBind -> Hs.CoreBind
--unTypeBind (Hs.NonRec name expr)    = Hs.NonRec name (unTypeExpr expr)
--unTypeBind (Hs.Rec [(name,expr)])   = Hs.Rec [(name, unTypeExpr expr)]
--unTypeBind (Hs.Rec _)               = error "unTypeBind: more than one binding in Rec"

-- Remove type information from expressions

--unTypeExpr :: Hs.CoreExpr -> Hs.CoreExpr
--unTypeExpr (Hs.App f (Hs.Type _))       = unTypeExpr f
--unTypeExpr (Hs.App f arg)               = Hs.App (unTypeExpr f) (unTypeExpr arg)
--unTypeExpr (Hs.Lam var rhs)
--    | isTyVar var                       = unTypeExpr rhs
--    | otherwise                         = Hs.Lam var (unTypeExpr rhs)
--unTypeExpr (Hs.Case expr var typ alts)  = Hs.Case (unTypeExpr expr) var typ (map unTypeAlt alts)
--unTypeExpr (Hs.Let _ _)                 = error "unTypeExpr: Let not supported"
--unTypeExpr (Hs.Cast _ _)                = error "unTypeExpr: cast not implemented"
--unTypeExpr (Hs.Note _ _)                = error "unTypeExpr: note not implemented"
--unTypeExpr e                            = e

--unTypeAlt :: Hs.CoreAlt -> Hs.CoreAlt
--unTypeAlt (con, vars, expr) = (con, vars, unTypeExpr expr)






--
-- The function freeVars calculates the free variables of an expression
--

--freeVars :: Hs.CoreExpr -> [Hs.CoreBndr]
--freeVars = nub . freeVars'

--freeVars' :: Hs.CoreExpr -> [Hs.CoreBndr]
--freeVars' (Hs.Var var)
--    | isLocalVar var                = [var]
--    | otherwise                     = []
--freeVars' (Hs.App fun arg)          = freeVars' fun ++ freeVars' arg
--freeVars' (Hs.Lam var expr)         = delete var (freeVars' expr)
--freeVars' (Hs.Case expr var _ alts) = freeVars' expr ++ delete var (concatMap freeVarsInAlt alts)
--freeVars' _                         = []

--freeVarsInAlt :: Hs.CoreAlt -> [Hs.CoreBndr]
--freeVarsInAlt (_, vars, rhs)    = freeVars rhs \\ vars






--
-- The function flatMatching takes a module and yields a module where
-- every function does only perform flat pattern matching. Furthermore,
-- all lambda functions are lifted.
--

--
-- Right new we generate a new function for every case expression. in fact,
-- we only have to generate a new function if ???
--

--flatMatching :: (MonadUnique m) => CoreModule -> m CoreModule
--flatMatching (CoreModule name typeenv binds imports) = do
--    bindss <- mapM (flatMatchingInBind (Hs.bindersOfBinds binds)) binds
--    return (CoreModule name typeenv (concat bindss) imports) 

--flatMatchingInBind :: (MonadUnique m) => [Hs.CoreBndr] -> Hs.CoreBind -> m [Hs.CoreBind]
--flatMatchingInBind binders (Hs.NonRec name expr)    = flatMatchingInBind' binders name expr False
--flatMatchingInBind binders (Hs.Rec [(name, expr)])  = flatMatchingInBind' binders name expr True
--flatMatchingInBind _ (Hs.Rec _)                     = error "flatMatchingInBind: rec with multiple bindings"

--flatMatchingInBind' :: (MonadUnique m) => [Hs.CoreBndr] -> Hs.CoreBndr -> Hs.CoreExpr -> Bool -> m [Hs.CoreBind]
--flatMatchingInBind' binders name expr rec = do
--    (rhs', binds) <- runWriterT (flatMatchingInExpr binders name rhs)
--    let expr' = mkCoreLams args rhs'
--    return (binds ++ [if rec then Hs.Rec [(name,expr')] else Hs.NonRec name expr'])
--    where
--    (args, rhs) = Hs.CollectBinders expr


--
-- Transformation
--

--hs2py :: CoreModule -> Py.ModuleSpan
--hs2py (CoreModule _ _ binds _) =
--    Py.Modue ( [pyImport [mod] | mod <- ["haskell", "syntax"]] )


-- writePython file

--writePyProg :: FilePath -> Py.ModuleSpan -> IO ()
--writePyProg file prog =
--    writeFile file (Py.prettyText prog)






-- unType remove all type information from CoreModule

-- unType :: CoreModule -> CoreModule
--unType (CoreModule name typeenv binds imports) = CoreModule name typeenv (map unTypeBinds) imports


--unTypeBind :: Hs.CoreBind -> Hs.CoreBind



