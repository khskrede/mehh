

import GHC
--GHC.Paths is available via cabal install ghc-paths
import GHC.Paths ( libdir )

import DynFlags ( defaultDynFlags )
import System.Environment ( getArgs )
import Outputable

import Text.JSON
import Text.JSON.Pretty
import Text.JSON.Generic
import Text.PrettyPrint.HughesPJ

import UniqFM
import Unique
import HscTypes
import CoreSyn

main = do
    (inFile:outFile:_) <- getArgs
    res <- compile inFile
    writeFile outFile $ show $ pp_value $ coreModToJS res


-- Process cabal file












-- Compile Haskell file

compile inFile = runGhc (Just libdir) $ do
    sdflags <- getSessionDynFlags
    let sdflags' = sdflags
    setSessionDynFlags sdflags'
    core <- compileToCoreSimplified inFile
    return $ (core)


coreModToJS :: CoreModule -> JSValue
coreModToJS (CoreModule name types binds) = 
    JSObject $ toJSObject $
    [( "%module", toJSON $ showSDoc $ ppr name ),
     ( "tdefg", typesToJS types),
     ( "binds", bindsToJS binds)]


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

