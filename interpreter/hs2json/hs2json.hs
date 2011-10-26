
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
import NameEnv
import UniqFM
import Unique

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
        writeFile outFile (show $ pp_value $ getJS core)

getCore :: FilePath -> IO CoreModule
getCore path = runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags dflags
                -- parse, typecheck, desugar, simplify
                compileToCoreSimplified path 


-- Get JSON representation
getJS :: CoreModule -> JSValue
getJS (CoreModule name typeenv binds imports) =
    JSObject $ toJSObject 
        [("%module", getNameJS name), 
         ("tdefg", getTypeEnvJS typeenv), 
         ("vdefg", getBindsJS binds)] 
        -- ("Imports", getImportsJS imports)] -- In addition to stuff from grammar. ?


-- Generate JSValue from Module
getNameJS :: Module -> JSValue
getNameJS mod = JSObject $ toJSObject 
    [("Name", toJSON $ moduleNameString $ moduleName mod),
     ("Package", toJSON $ packageIdString $ modulePackageId mod )]


-- Generate JSValue from imports
getImportsJS :: [Module] -> JSValue
getImportsJS = JSArray . map getNameJS


-- Generate JSValue from type TypeEnv
getTypeEnvJS :: TypeEnv -> JSValue
getTypeEnvJS x = JSObject $ toJSObject $ map getUniqueJS (ufmToList x)  -- TODO ??

getUniqueJS :: (Unique, TyThing) -> (String, JSValue)
getUniqueJS (str, thing) = 
    (getUniqueString str, getTyThingJS thing) -- TODO ??

getUniqueString :: Unique -> String
getUniqueString = showSDoc . ppr -- TODO ??


-- Generate JSValue from TyThing
getTyThingJS :: TyThing -> JSValue
getTyThingJS (AnId i) = 
    getVarJS i -- TODO
getTyThingJS (ADataCon d) = 
    JSNull -- TODO
getTyThingJS (ATyCon t) = 
    JSNull -- TODO
getTyThingJS (AClass c) = 
    JSNull -- TODO


-- Generate JSValue from binds
getBindsJS :: [Hs.CoreBind] -> JSValue
getBindsJS = JSArray . map getBindJS

getBindJS :: Hs.CoreBind -> JSValue
getBindJS (Hs.NonRec name expr) =
    getExprJS expr -- TODO ??
getBindJS (Hs.Rec [(name, expr)]) =
    getExprJS expr -- TODO ??
getBindJS (Hs.Rec _) =
    error "getBindString"


-- Generate JSValue from expression
getExprJS :: Hs.CoreExpr -> JSValue
getExprJS (Hs.App f arg) = 
    JSObject $ toJSObject
    [("App", JSObject $ toJSObject [("Expr", getExprJS f)]),
     ("Args", getExprJS arg)]
getExprJS (Hs.Lam var rhs) = 
    JSObject $ toJSObject [("Lam", JSNull)] -- TODO ??
getExprJS (Hs.Case expr var typ alts) = 
    JSObject $ toJSObject [("Case", JSNull)] -- TODO ??
getExprJS (Hs.Let _ _) = 
    JSObject $ toJSObject [("Let", JSNull)] -- TODO ??
getExprJS (Hs.Cast _ _) = 
    JSObject $ toJSObject [("Cast", JSNull)] -- TODO ??
getExprJS (Hs.Note _ _) = 
    JSObject $ toJSObject [("Note", JSNull)] -- TODO ??
getExprJS (Hs.Type t) =
    JSObject $ toJSObject [("Type", toJSON $ showSDoc $ ppr t)] -- TODO ??
getExprJS (Hs.Var var) =
    JSObject $ toJSObject [("Var", toJSON $ getOccString var)] -- TODO ??
getExprJS (Hs.Lit lit) =
    JSObject $ toJSObject [("Lit", getLitJS lit)] -- TODO ??


-- Convert any literal to its JSON equivalent
getLitJS :: Literal -> JSValue
getLitJS (MachChar char) = 
    JSObject $ toJSObject [("Char", toJSON char)]
getLitJS (MachStr str) = 
    JSObject $ toJSObject [("String", toJSON $ unpackFS str)]
getLitJS (MachNullAddr) = 
    JSObject $ toJSObject [("Null", JSNull)]
getLitJS (MachInt int) = 
    JSObject $ toJSObject [("Int", toJSON int)]
getLitJS (MachInt64 int) = 
    JSObject $ toJSObject [("Int64", toJSON int)]
getLitJS (MachWord word) = 
    JSObject $ toJSObject [("Word", toJSON word)]
getLitJS (MachWord64 word) = 
    JSObject $ toJSObject [("Word64", toJSON word)]
getLitJS (MachFloat float) = 
    JSObject $ toJSObject [("Float", toJSON float)]
getLitJS (MachDouble double) = 
    JSObject $ toJSObject [("Double", toJSON double)]
getLitJS (MachLabel str may fod) = 
    JSObject $ toJSObject [("Label", toJSON $ unpackFS str)] -- TODO


getVarJS :: Var -> JSValue
getVarJS var = JSNull -- TODO

-- FunctionOrData to JSON
--getFuncOrData :: FunctionOrData -> JSValue
--getFuncOrData IsFunction = 
--    toJSON "Function"
--getFuncOrData isData = 
--    toJSON "Data"
