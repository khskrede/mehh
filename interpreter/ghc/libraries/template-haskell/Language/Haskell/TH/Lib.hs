-- |
-- TH.Lib contains lots of useful helper functions for
-- generating and manipulating Template Haskell terms

module Language.Haskell.TH.Lib where
    -- All of the exports from this module should
    -- be "public" functions.  The main module TH
    -- re-exports them all.

import Language.Haskell.TH.Syntax
import Control.Monad( liftM, liftM2 )

----------------------------------------------------------
-- * Type synonyms
----------------------------------------------------------

type InfoQ          = Q Info
type PatQ           = Q Pat
type FieldPatQ      = Q FieldPat
type ExpQ           = Q Exp
type DecQ           = Q Dec
type DecsQ          = Q [Dec]
type ConQ           = Q Con
type TypeQ          = Q Type
type CxtQ           = Q Cxt
type PredQ          = Q Pred
type MatchQ         = Q Match
type ClauseQ        = Q Clause
type BodyQ          = Q Body
type GuardQ         = Q Guard
type StmtQ          = Q Stmt
type RangeQ         = Q Range
type StrictTypeQ    = Q StrictType
type VarStrictTypeQ = Q VarStrictType
type FieldExpQ      = Q FieldExp
type InlineSpecQ    = Q InlineSpec

----------------------------------------------------------
-- * Lowercase pattern syntax functions
----------------------------------------------------------

intPrimL    :: Integer -> Lit
intPrimL    = IntPrimL
wordPrimL    :: Integer -> Lit
wordPrimL    = WordPrimL
floatPrimL  :: Rational -> Lit
floatPrimL  = FloatPrimL
doublePrimL :: Rational -> Lit
doublePrimL = DoublePrimL
integerL    :: Integer -> Lit
integerL    = IntegerL
charL       :: Char -> Lit
charL       = CharL
stringL     :: String -> Lit
stringL     = StringL
stringPrimL :: String -> Lit
stringPrimL = StringPrimL
rationalL   :: Rational -> Lit
rationalL   = RationalL

litP :: Lit -> PatQ
litP l = return (LitP l)
varP :: Name -> PatQ
varP v = return (VarP v)
tupP :: [PatQ] -> PatQ
tupP ps = do { ps1 <- sequence ps; return (TupP ps1)}
conP :: Name -> [PatQ] -> PatQ
conP n ps = do ps' <- sequence ps
               return (ConP n ps')
infixP :: PatQ -> Name -> PatQ -> PatQ
infixP p1 n p2 = do p1' <- p1
                    p2' <- p2
                    return (InfixP p1' n p2')
tildeP :: PatQ -> PatQ
tildeP p = do p' <- p
              return (TildeP p')
bangP :: PatQ -> PatQ
bangP p = do p' <- p
             return (BangP p')
asP :: Name -> PatQ -> PatQ
asP n p = do p' <- p
             return (AsP n p')
wildP :: PatQ
wildP = return WildP
recP :: Name -> [FieldPatQ] -> PatQ
recP n fps = do fps' <- sequence fps
                return (RecP n fps')
listP :: [PatQ] -> PatQ
listP ps = do ps' <- sequence ps
              return (ListP ps')
sigP :: PatQ -> TypeQ -> PatQ
sigP p t = do p' <- p
              t' <- t
              return (SigP p' t')
viewP :: ExpQ -> PatQ -> PatQ
viewP e p = do e' <- e
               p' <- p
               return (ViewP e' p')

fieldPat :: Name -> PatQ -> FieldPatQ
fieldPat n p = do p' <- p
                  return (n, p')


-------------------------------------------------------------------------------
-- *   Stmt

bindS :: PatQ -> ExpQ -> StmtQ
bindS p e = liftM2 BindS p e

letS :: [DecQ] -> StmtQ
letS ds = do { ds1 <- sequence ds; return (LetS ds1) }

noBindS :: ExpQ -> StmtQ
noBindS e = do { e1 <- e; return (NoBindS e1) }

parS :: [[StmtQ]] -> StmtQ
parS _ = fail "No parallel comprehensions yet"

-------------------------------------------------------------------------------
-- *   Range

fromR :: ExpQ -> RangeQ
fromR x = do { a <- x; return (FromR a) }  

fromThenR :: ExpQ -> ExpQ -> RangeQ
fromThenR x y = do { a <- x; b <- y; return (FromThenR a b) }  

fromToR :: ExpQ -> ExpQ -> RangeQ
fromToR x y = do { a <- x; b <- y; return (FromToR a b) }  

fromThenToR :: ExpQ -> ExpQ -> ExpQ -> RangeQ
fromThenToR x y z = do { a <- x; b <- y; c <- z;
                         return (FromThenToR a b c) }  
-------------------------------------------------------------------------------
-- *   Body

normalB :: ExpQ -> BodyQ
normalB e = do { e1 <- e; return (NormalB e1) }

guardedB :: [Q (Guard,Exp)] -> BodyQ
guardedB ges = do { ges' <- sequence ges; return (GuardedB ges') }

-------------------------------------------------------------------------------
-- *   Guard

normalG :: ExpQ -> GuardQ
normalG e = do { e1 <- e; return (NormalG e1) }

normalGE :: ExpQ -> ExpQ -> Q (Guard, Exp)
normalGE g e = do { g1 <- g; e1 <- e; return (NormalG g1, e1) }

patG :: [StmtQ] -> GuardQ
patG ss = do { ss' <- sequence ss; return (PatG ss') }

patGE :: [StmtQ] -> ExpQ -> Q (Guard, Exp)
patGE ss e = do { ss' <- sequence ss;
		  e'  <- e;
                  return (PatG ss', e') }

-------------------------------------------------------------------------------
-- *   Match and Clause

-- | Use with 'caseE'
match :: PatQ -> BodyQ -> [DecQ] -> MatchQ
match p rhs ds = do { p' <- p;
                      r' <- rhs;
                      ds' <- sequence ds;
                      return (Match p' r' ds') }

-- | Use with 'funD'
clause :: [PatQ] -> BodyQ -> [DecQ] -> ClauseQ
clause ps r ds = do { ps' <- sequence ps;
                      r' <- r;
                      ds' <- sequence ds;
                      return (Clause ps' r' ds') }


---------------------------------------------------------------------------
-- *   Exp

-- | Dynamically binding a variable (unhygenic)
dyn :: String -> Q Exp 
dyn s = return (VarE (mkName s))

global :: Name -> ExpQ
global s = return (VarE s)

varE :: Name -> ExpQ
varE s = return (VarE s)

conE :: Name -> ExpQ
conE s =  return (ConE s)

litE :: Lit -> ExpQ
litE c = return (LitE c)

appE :: ExpQ -> ExpQ -> ExpQ
appE x y = do { a <- x; b <- y; return (AppE a b)}

infixE :: Maybe ExpQ -> ExpQ -> Maybe ExpQ -> ExpQ
infixE (Just x) s (Just y) = do { a <- x; s' <- s; b <- y;
                                  return (InfixE (Just a) s' (Just b))}
infixE Nothing  s (Just y) = do { s' <- s; b <- y;
                                  return (InfixE Nothing s' (Just b))}
infixE (Just x) s Nothing  = do { a <- x; s' <- s;
                                  return (InfixE (Just a) s' Nothing)}
infixE Nothing  s Nothing  = do { s' <- s; return (InfixE Nothing s' Nothing) }

infixApp :: ExpQ -> ExpQ -> ExpQ -> ExpQ
infixApp x y z = infixE (Just x) y (Just z)
sectionL :: ExpQ -> ExpQ -> ExpQ
sectionL x y = infixE (Just x) y Nothing
sectionR :: ExpQ -> ExpQ -> ExpQ
sectionR x y = infixE Nothing x (Just y)

lamE :: [PatQ] -> ExpQ -> ExpQ
lamE ps e = do ps' <- sequence ps
               e' <- e
               return (LamE ps' e')

-- | Single-arg lambda
lam1E :: PatQ -> ExpQ -> ExpQ
lam1E p e = lamE [p] e

tupE :: [ExpQ] -> ExpQ
tupE es = do { es1 <- sequence es; return (TupE es1)}

condE :: ExpQ -> ExpQ -> ExpQ -> ExpQ
condE x y z =  do { a <- x; b <- y; c <- z; return (CondE a b c)}

letE :: [DecQ] -> ExpQ -> ExpQ
letE ds e = do { ds2 <- sequence ds; e2 <- e; return (LetE ds2 e2) }

caseE :: ExpQ -> [MatchQ] -> ExpQ
caseE e ms = do { e1 <- e; ms1 <- sequence ms; return (CaseE e1 ms1) } 

doE :: [StmtQ] -> ExpQ
doE ss = do { ss1 <- sequence ss; return (DoE ss1) } 

compE :: [StmtQ] -> ExpQ
compE ss = do { ss1 <- sequence ss; return (CompE ss1) } 

arithSeqE :: RangeQ -> ExpQ
arithSeqE r = do { r' <- r; return (ArithSeqE r') }  

listE :: [ExpQ] -> ExpQ
listE es = do { es1 <- sequence es; return (ListE es1) }

sigE :: ExpQ -> TypeQ -> ExpQ
sigE e t = do { e1 <- e; t1 <- t; return (SigE e1 t1) }

recConE :: Name -> [Q (Name,Exp)] -> ExpQ
recConE c fs = do { flds <- sequence fs; return (RecConE c flds) }

recUpdE :: ExpQ -> [Q (Name,Exp)] -> ExpQ
recUpdE e fs = do { e1 <- e; flds <- sequence fs; return (RecUpdE e1 flds) }

stringE :: String -> ExpQ
stringE = litE . stringL

fieldExp :: Name -> ExpQ -> Q (Name, Exp)
fieldExp s e = do { e' <- e; return (s,e') }

-- ** 'arithSeqE' Shortcuts
fromE :: ExpQ -> ExpQ
fromE x = do { a <- x; return (ArithSeqE (FromR a)) }  

fromThenE :: ExpQ -> ExpQ -> ExpQ
fromThenE x y = do { a <- x; b <- y; return (ArithSeqE (FromThenR a b)) }  

fromToE :: ExpQ -> ExpQ -> ExpQ
fromToE x y = do { a <- x; b <- y; return (ArithSeqE (FromToR a b)) }  

fromThenToE :: ExpQ -> ExpQ -> ExpQ -> ExpQ
fromThenToE x y z = do { a <- x; b <- y; c <- z;
                         return (ArithSeqE (FromThenToR a b c)) }  


-------------------------------------------------------------------------------
-- *   Dec

valD :: PatQ -> BodyQ -> [DecQ] -> DecQ
valD p b ds = 
  do { p' <- p
     ; ds' <- sequence ds
     ; b' <- b
     ; return (ValD p' b' ds')
     }

funD :: Name -> [ClauseQ] -> DecQ
funD nm cs = 
 do { cs1 <- sequence cs
    ; return (FunD nm cs1)
    }

tySynD :: Name -> [TyVarBndr] -> TypeQ -> DecQ
tySynD tc tvs rhs = do { rhs1 <- rhs; return (TySynD tc tvs rhs1) }

dataD :: CxtQ -> Name -> [TyVarBndr] -> [ConQ] -> [Name] -> DecQ
dataD ctxt tc tvs cons derivs =
  do
    ctxt1 <- ctxt
    cons1 <- sequence cons
    return (DataD ctxt1 tc tvs cons1 derivs)

newtypeD :: CxtQ -> Name -> [TyVarBndr] -> ConQ -> [Name] -> DecQ
newtypeD ctxt tc tvs con derivs =
  do
    ctxt1 <- ctxt
    con1 <- con
    return (NewtypeD ctxt1 tc tvs con1 derivs)

classD :: CxtQ -> Name -> [TyVarBndr] -> [FunDep] -> [DecQ] -> DecQ
classD ctxt cls tvs fds decs =
  do 
    decs1 <- sequence decs
    ctxt1 <- ctxt
    return $ ClassD ctxt1 cls tvs fds decs1

instanceD :: CxtQ -> TypeQ -> [DecQ] -> DecQ
instanceD ctxt ty decs =
  do 
    ctxt1 <- ctxt
    decs1 <- sequence decs
    ty1   <- ty
    return $ InstanceD ctxt1 ty1 decs1

sigD :: Name -> TypeQ -> DecQ
sigD fun ty = liftM (SigD fun) $ ty

forImpD :: Callconv -> Safety -> String -> Name -> TypeQ -> DecQ
forImpD cc s str n ty
 = do ty' <- ty
      return $ ForeignD (ImportF cc s str n ty')

pragInlD :: Name -> InlineSpecQ -> DecQ
pragInlD n ispec 
  = do
      ispec1 <- ispec 
      return $ PragmaD (InlineP n ispec1)

pragSpecD :: Name -> TypeQ -> DecQ
pragSpecD n ty
  = do
      ty1    <- ty
      return $ PragmaD (SpecialiseP n ty1 Nothing)

pragSpecInlD :: Name -> TypeQ -> InlineSpecQ -> DecQ
pragSpecInlD n ty ispec 
  = do
      ty1    <- ty
      ispec1 <- ispec
      return $ PragmaD (SpecialiseP n ty1 (Just ispec1))

familyNoKindD :: FamFlavour -> Name -> [TyVarBndr] -> DecQ
familyNoKindD flav tc tvs = return $ FamilyD flav tc tvs Nothing

familyKindD :: FamFlavour -> Name -> [TyVarBndr] -> Kind -> DecQ
familyKindD flav tc tvs k = return $ FamilyD flav tc tvs (Just k)

dataInstD :: CxtQ -> Name -> [TypeQ] -> [ConQ] -> [Name] -> DecQ
dataInstD ctxt tc tys cons derivs =
  do
    ctxt1 <- ctxt
    tys1  <- sequence tys
    cons1 <- sequence cons
    return (DataInstD ctxt1 tc tys1 cons1 derivs)

newtypeInstD :: CxtQ -> Name -> [TypeQ] -> ConQ -> [Name] -> DecQ
newtypeInstD ctxt tc tys con derivs =
  do
    ctxt1 <- ctxt
    tys1  <- sequence tys
    con1  <- con
    return (NewtypeInstD ctxt1 tc tys1 con1 derivs)

tySynInstD :: Name -> [TypeQ] -> TypeQ -> DecQ
tySynInstD tc tys rhs = 
  do 
    tys1 <- sequence tys
    rhs1 <- rhs
    return (TySynInstD tc tys1 rhs1)

cxt :: [PredQ] -> CxtQ
cxt = sequence

classP :: Name -> [TypeQ] -> PredQ
classP cla tys
  = do
      tys1 <- sequence tys
      return (ClassP cla tys1)

equalP :: TypeQ -> TypeQ -> PredQ
equalP tleft tright
  = do
      tleft1  <- tleft
      tright1 <- tright
      return (EqualP tleft1 tright1)

normalC :: Name -> [StrictTypeQ] -> ConQ
normalC con strtys = liftM (NormalC con) $ sequence strtys

recC :: Name -> [VarStrictTypeQ] -> ConQ
recC con varstrtys = liftM (RecC con) $ sequence varstrtys

infixC :: Q (Strict, Type) -> Name -> Q (Strict, Type) -> ConQ
infixC st1 con st2 = do st1' <- st1
                        st2' <- st2
                        return $ InfixC st1' con st2'

forallC :: [TyVarBndr] -> CxtQ -> ConQ -> ConQ
forallC ns ctxt con = liftM2 (ForallC ns) ctxt con


-------------------------------------------------------------------------------
-- *   Type

forallT :: [TyVarBndr] -> CxtQ -> TypeQ -> TypeQ
forallT tvars ctxt ty = do
    ctxt1 <- ctxt
    ty1   <- ty
    return $ ForallT tvars ctxt1 ty1

varT :: Name -> TypeQ
varT = return . VarT

conT :: Name -> TypeQ
conT = return . ConT

appT :: TypeQ -> TypeQ -> TypeQ
appT t1 t2 = do
           t1' <- t1
           t2' <- t2
           return $ AppT t1' t2'

arrowT :: TypeQ
arrowT = return ArrowT

listT :: TypeQ
listT = return ListT

tupleT :: Int -> TypeQ
tupleT i = return (TupleT i)

sigT :: TypeQ -> Kind -> TypeQ
sigT t k
  = do
      t' <- t
      return $ SigT t' k

isStrict, notStrict :: Q Strict
isStrict = return $ IsStrict
notStrict = return $ NotStrict

strictType :: Q Strict -> TypeQ -> StrictTypeQ
strictType = liftM2 (,)

varStrictType :: Name -> StrictTypeQ -> VarStrictTypeQ
varStrictType v st = do (s, t) <- st
                        return (v, s, t)

-------------------------------------------------------------------------------
-- *   Kind

plainTV :: Name -> TyVarBndr
plainTV = PlainTV

kindedTV :: Name -> Kind -> TyVarBndr
kindedTV = KindedTV

starK :: Kind
starK = StarK

arrowK :: Kind -> Kind -> Kind
arrowK = ArrowK

-------------------------------------------------------------------------------
-- *   Callconv

cCall, stdCall :: Callconv
cCall = CCall
stdCall = StdCall

-------------------------------------------------------------------------------
-- *   Safety

unsafe, safe, threadsafe :: Safety
unsafe = Unsafe
safe = Safe
threadsafe = Threadsafe

-------------------------------------------------------------------------------
-- *   InlineSpec

inlineSpecNoPhase :: Bool -> Bool -> InlineSpecQ
inlineSpecNoPhase inline conlike
  = return $ InlineSpec inline conlike Nothing

inlineSpecPhase :: Bool -> Bool -> Bool -> Int -> InlineSpecQ
inlineSpecPhase inline conlike beforeFrom phase
  = return $ InlineSpec inline conlike (Just (beforeFrom, phase))

-------------------------------------------------------------------------------
-- *   FunDep

funDep :: [Name] -> [Name] -> FunDep
funDep = FunDep

-------------------------------------------------------------------------------
-- *   FamFlavour

typeFam, dataFam :: FamFlavour
typeFam = TypeFam
dataFam = DataFam

--------------------------------------------------------------
-- * Useful helper functions

combine :: [([(Name, Name)], Pat)] -> ([(Name, Name)], [Pat])
combine pairs = foldr f ([],[]) pairs
  where f (env,p) (es,ps) = (env++es,p:ps)

rename :: Pat -> Q ([(Name, Name)], Pat)
rename (LitP c)  = return([],LitP c)
rename (VarP s)  = do { s1 <- newName (nameBase s); return([(s,s1)],VarP s1) }
rename (TupP pats) = do { pairs <- mapM rename pats; g(combine pairs) }
   where g (es,ps) = return (es,TupP ps)
rename (ConP nm pats) = do { pairs <- mapM rename pats; g(combine pairs) }
   where g (es,ps) = return (es,ConP nm ps)
rename (InfixP p1 n p2) = do { r1 <- rename p1;
                               r2 <- rename p2;
                               let {(env, [p1', p2']) = combine [r1, r2]};
                               return (env, InfixP p1' n p2') }
rename (TildeP p) = do { (env,p2) <- rename p; return(env,TildeP p2) }   
rename (BangP p) = do { (env,p2) <- rename p; return(env,BangP p2) }   
rename (AsP s p) = 
   do { s1 <- newName (nameBase s); (env,p2) <- rename p; return((s,s1):env,AsP s1 p2) }
rename WildP = return([],WildP)
rename (RecP nm fs) = do { pairs <- mapM rename ps; g(combine pairs) }
    where g (env,ps') = return (env,RecP nm (zip ss ps'))
          (ss,ps) = unzip fs
rename (ListP pats) = do { pairs <- mapM rename pats; g(combine pairs) }
   where g (es,ps) = return (es,ListP ps)
rename (SigP {}) = fail "rename: Don't know how to do SigP yet"
rename (ViewP {}) = fail "rename: Don't know how to do ViewP yet"

genpat :: Pat -> Q ((Name -> ExpQ), Pat)
genpat p = do { (env,p2) <- rename p; return (alpha env,p2) }

alpha :: [(Name, Name)] -> Name -> ExpQ
alpha env s = case lookup s env of
               Just x -> varE x
               Nothing -> varE s

appsE :: [ExpQ] -> ExpQ
appsE [] = error "appsE []"
appsE [x] = x
appsE (x:y:zs) = appsE ( (appE x y) : zs )

simpleMatch :: Pat -> Exp -> Match
simpleMatch p e = Match p (NormalB e) []

