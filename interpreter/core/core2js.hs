

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
    writeFile outFile $ show $ pp_value $ getJS c


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
         ("ty", tyJS ty)]


--
-- Constructor definition
--

cdefJS :: Cdef -> JSValue
cdefJS (Constr qual tbinds tys) = 
    JSObject $ toJSObject
        [("qdcon", qdconJS qual),
         ("tbind", JSArray $ map tbindJS tbinds),
         ("aty", JSArray $ map tyJS tys)]


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
-- Atomic expression && Expression && Argument
--

expJS :: Exp -> JSValue

expJS (Var qvar) = qvarJS qvar

expJS (Dcon qdcon) = qdconJS qdcon

expJS (Lit lit) = litJS lit

expJS (App exp1 exp2) =
    JSObject $ toJSObject
        [("aexp", expJS exp1),
         ("args", expJS exp2)] --todo ?

expJS (Appt exp ty) =
    JSObject $ toJSObject
        [("aexp", expJS exp),
         ("args", tyJS ty)] -- todo ?

expJS (Lam bind exp) =
    JSObject $ toJSObject
        [("lambda", binderJS bind),
         ("exp", expJS exp)] --todo ?

expJS (Let vdefg exp) =
    JSObject $ toJSObject
        [("%let", vdefgJS vdefg),
         ("%in", expJS exp)]

expJS (Case exp vbind ty alts) =
    JSObject $ toJSObject
        [("%case", tyJS ty),
         ("exp", expJS exp),
         ("%of", vbindJS vbind),
         ("alt", JSArray $ map altJS alts)]

expJS (Cast exp ty) =
    JSObject $ toJSObject
        [("%cast", expJS exp),
         ("aty", tyJS ty)]

expJS (Note string exp) =
    JSObject $ toJSObject
        [("%note", toJSON string),
         ("exp", expJS exp)]

expJS (External string ty) =
    JSObject $ toJSObject
        [("%external ccal", toJSON string), 
         ("aty", tyJS ty)]


--
-- Argument
--



--
-- Case alternative
--


altJS :: Alt -> JSValue
altJS (Acon qdcon tbinds vbinds exp) =
    JSObject $ toJSObject
        [("qdcon", qdconJS qdcon),
         ("tbind", JSArray $ map tbindJS tbinds),
         ("vbind", JSArray $ map vbindJS vbinds),
         ("exp", expJS exp)]

altJS (Alit lit exp) =
    JSObject $ toJSObject
        [("lit", litJS lit),
         ("exp", expJS exp)]

altJS (Adefault exp) =
    JSObject $ toJSObject
        [("%_", expJS exp)]


--
-- Binder
--

binderJS :: Bind -> JSValue
binderJS (Vb vbind) =
    JSObject $ toJSObject
        [("vbind", vbindJS vbind)]
binderJS (Tb tbind) = 
    JSObject $ toJSObject
        [("tbind", tbindJS tbind)]


--
-- Type binder
--

tbindJS :: Tbind -> JSValue
tbindJS (tvar, kind) =
    JSObject $ toJSObject
        [("tyvar", tyvarJS tvar),
         ("kind", kindJS kind)]


--
-- Value binder
--

vbindJS :: Vbind -> JSValue
vbindJS (var, ty) =
    JSObject $ toJSObject
        [("var", varJS var),
         ("ty", tyJS ty)]


--
-- Literal
--

litJS :: Lit -> JSValue
litJS (Literal corelit ty) = corelitJS corelit --todo?

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


