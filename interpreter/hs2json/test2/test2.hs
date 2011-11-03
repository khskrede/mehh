

-- Core utilities

import Language.Core.ParsecParser
import Language.Core.Core
import Language.Core.Printer

import Text.ParserCombinators.Parsec.Error

-- IO utilities

import System                       ( getArgs )

-- JSON utilities

import Text.JSON
import Text.JSON.Pretty
import Text.JSON.Generic


main = do 
    (inFile:outFile:_) <- getArgs
    compile inFile outFile


compile :: FilePath -> FilePath -> IO ()
compile inFile outFile = do
    c <- parseCore inFile
    putStrLn $ show $ pp_value $ getJS c


getJS :: (Either ParseError Module) -> JSValue
getJS (Left err) = toJSON $ show err
getJS (Right mod) = modJS mod


--
-- Module
--

modJS :: Module -> JSValue
modJS (Module anmname tdefg vdefg) =
    JSObject $ toJSObject
        [("%module", anmnameJS anmname ),
         ("tdefg", JSArray $ map tdefgJS tdefg),
         ("vdefg", JSArray $ map vdefgJS vdefg)]

--
-- Type definition
--

tdefgJS :: Tdef -> JSValue
tdefgJS (Data tcon tbinds cdefs) = 
    JSObject $ toJSObject
        [("%data", qtyconJS tcon),
         ("tbind", JSArray $ map tbindJS tbinds),
         ("cdef", JSArray $ map cdefJS cdefs)]
tdefgJS (Newtype tcon1 tcon2 tbind ty) = 
    JSObject $ toJSObject
        [("%newtype", qtyconJS tcon1), 
         ("qtycon", qtyconJS tcon2), 
         ("tbind", JSArray $ map tbindJS tbind),
         ("ty", toJSON $ show ty)] --todo


--
-- Constructor definition
--

cdefJS :: Cdef -> JSValue
cdefJS (Constr qual tbinds tys) = 
    JSObject $ toJSObject
        [("qdcon", qdconJS qual),
         ("tbind", JSArray $ map tbindJS tbinds),
         ("aty", toJSON $ show tys)] --todo


--
-- Value definition
--

vdefgJS :: Vdefg -> JSValue
vdefgJS (Rec vdefs) = 
    JSObject $ toJSObject 
        [("%rec", JSArray $ map vdefJS vdefs)]
vdefgJS (Nonrec vdef) = vdefJS vdef

vdefJS :: Vdef -> JSValue
vdefJS (Vdef (qvar, ty, exp)) =
    JSObject $ toJSObject
        [("qvar", qvarJS qvar),
         ("ty", tyJS ty),
         ("exp", expJS exp)]


--
-- Atomic expression && Expression
--

expJS :: Exp -> JSValue

expJS (Var qvar) = qvarJS qvar

expJS (Dcon qdcon) = qdconJS qdcon

expJS (Lit lit) = litJS lit

expJS (App exp1 exp2) =
    JSObject $ toJSObject
        [("aexp", expJS exp1),
         ("args", JSNull)] --todo

expJS (Appt exp ty) = JSNull --todo

expJS (Lam bind exp) =
    JSObject $ toJSObject
        [("lambda", JSNull)] --todo

expJS (Let vdefg exp) = JSNull --todo

expJS (Case exp vbind ty alts) = JSNull --todo

expJS (Cast exp ty) = JSNull --todo

expJS (Note string exp) = JSNull --todo

expJS (External string ty) = JSNull --todo


--
-- Argument
--



--
-- Case alternative
--



--
-- Binder
--



--
-- Type binder
--

tbindJS :: Tbind -> JSValue
tbindJS (tvar, kind) =
    JSObject $ toJSObject
        [("tyvar", tyvarJS tvar),
         ("kind", kindJS kind)]


--
-- Literal
--

litJS :: Lit -> JSValue
litJS (Literal corelit ty) = corelitJS corelit

corelitJS :: CoreLit -> JSValue
corelitJS = toJSON


--
-- Atomic type && Basic type && Type
--

tyJS :: Ty -> JSValue
tyJS (Tvar tyvar) = tyvarJS tyvar 
tyJS (Tcon qtycon) = qtyconJS qtycon
tyJS (Tapp ty1 ty2) =
    JSObject $ toJSObject
        [("bty", tyJS ty1),
         ("aty", tyJS ty2)]
tyJS (Tforall tbind ty) =
    JSObject $ toJSObject
        [("%forall", tbindJS tbind),
         ("ty", tyJS ty)]
tyJS (TransCoercion ty1 ty2) =
    JSObject $ toJSObject
        [("%trans", tyJS ty1),
         ("aty", tyJS ty2)]
tyJS (SymCoercion ty) =
    JSObject $ toJSObject
        [("%sym", tyJS ty)]
tyJS (UnsafeCoercion ty1 ty2) =
    JSObject $ toJSObject
        [("%unsafe", tyJS ty1),
         ("aty", tyJS ty2)]
tyJS (InstCoercion ty1 ty2) =
    JSObject $ toJSObject
        [("%inst", tyJS ty1),
         ("aty", tyJS ty2)]
tyJS (LeftCoercion ty) =
    JSObject $ toJSObject
        [("%left", tyJS ty)]
tyJS (RightCoercion ty) =
    JSObject $ toJSObject
        [("%right", tyJS ty)]


--
-- Atomic kind && Kind
--

kindJS :: Kind -> JSValue
kindJS (Klifted) = toJSON "*"
kindJS (Kunlifted) = toJSON "#"
kindJS (Kopen) = toJSON "?"
kindJS (Karrow kind1 kind2) =
    JSObject $ toJSObject
        [("akind", kindJS kind1),
         ("kind", kindJS kind2)]
kindJS (Keq ty1 ty2) =
    JSObject $ toJSObject
        [("bty", tyJS ty1),
         ("bty", tyJS ty2)]


--
-- Identifier
-- 

anmnameJS :: AnMname -> JSValue
anmnameJS (M ((P pname), ids, id)) = toJSON $ (pname ++ ":" ++ (foldl (++) "" ids) ++ id)

tyconJS :: Tcon -> JSValue
tyconJS = toJSON

qtyconJS :: (Qual Tcon) -> JSValue
qtyconJS (Just a, str) = toJSON ( (show a) ++ "." ++ str )
qtyconJS (Nothing, str) = toJSON str

tyvarJS :: Tvar -> JSValue
tyvarJS = toJSON

dconJS :: Dcon -> JSValue
dconJS = toJSON

qdconJS :: (Qual Dcon) -> JSValue
qdconJS (Just a, str) = toJSON ( (show a) ++ "." ++ str )
qdconJS (Nothing, str) = toJSON str

varJS :: Var -> JSValue
varJS = toJSON

qvarJS :: (Qual Var) -> JSValue
qvarJS (Just a, str) = toJSON ( (show a) ++ "." ++ str )
qvarJS (Nothing, str) = toJSON str


