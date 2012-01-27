-- | contains a prettyprinter for the
-- Template Haskell datatypes

module Language.Haskell.TH.Ppr where
    -- All of the exports from this module should
    -- be "public" functions.  The main module TH
    -- re-exports them all.

import Text.PrettyPrint.HughesPJ (render)
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax
import Data.Char ( toLower )

nestDepth :: Int
nestDepth = 4

type Precedence = Int
appPrec, opPrec, noPrec :: Precedence
appPrec = 2    -- Argument of a function application
opPrec  = 1    -- Argument of an infix operator
noPrec  = 0    -- Others

parensIf :: Bool -> Doc -> Doc
parensIf True d = parens d
parensIf False d = d

------------------------------

pprint :: Ppr a => a -> String
pprint x = render $ to_HPJ_Doc $ ppr x

class Ppr a where
    ppr :: a -> Doc
    ppr_list :: [a] -> Doc
    ppr_list = vcat . map ppr

instance Ppr a => Ppr [a] where
    ppr x = ppr_list x

------------------------------
instance Ppr Name where
    ppr v = pprName v

------------------------------
instance Ppr Info where
    ppr (ClassI d is) = ppr d $$ vcat (map ppr is)
    ppr (TyConI d)    = ppr d
    ppr (PrimTyConI name arity is_unlifted) 
      = text "Primitive"
	<+> (if is_unlifted then text "unlifted" else empty)
	<+> text "type construtor" <+> quotes (ppr name)
	<+> parens (text "arity" <+> int arity)
    ppr (ClassOpI v ty cls fix) 
      = text "Class op from" <+> ppr cls <> colon <+>
        vcat [ppr_sig v ty, pprFixity v fix]
    ppr (DataConI v ty tc fix) 
      = text "Constructor from" <+> ppr tc <> colon <+>
        vcat [ppr_sig v ty, pprFixity v fix]
    ppr (TyVarI v ty)
      = text "Type variable" <+> ppr v <+> equals <+> ppr ty
    ppr (VarI v ty mb_d fix) 
      = vcat [ppr_sig v ty, pprFixity v fix, 
              case mb_d of { Nothing -> empty; Just d -> ppr d }]

instance Ppr ClassInstance where
  ppr (ClassInstance { ci_dfun = _dfun,
      		       ci_tvs  = _tvs,
      		       ci_cxt  = cxt,
      		       ci_cls  = cls,
                       ci_tys = tys })
    = text "instance" <+> pprCxt cxt 
      <+> ppr cls <+> sep (map pprParendType tys)

ppr_sig :: Name -> Type -> Doc
ppr_sig v ty = ppr v <+> text "::" <+> ppr ty

pprFixity :: Name -> Fixity -> Doc
pprFixity _ f | f == defaultFixity = empty
pprFixity v (Fixity i d) = ppr_fix d <+> int i <+> ppr v
    where ppr_fix InfixR = text "infixr"
          ppr_fix InfixL = text "infixl"
          ppr_fix InfixN = text "infix"


------------------------------
instance Ppr Exp where
    ppr = pprExp noPrec

pprInfixExp :: Exp -> Doc
pprInfixExp (VarE v) = pprName' Infix v
pprInfixExp (ConE v) = pprName' Infix v
pprInfixExp _        = error "Attempt to pretty-print non-variable or constructor in infix context!"

pprExp :: Precedence -> Exp -> Doc
pprExp _ (VarE v)     = pprName' Applied v
pprExp _ (ConE c)     = pprName' Applied c
pprExp i (LitE l)     = pprLit i l
pprExp i (AppE e1 e2) = parensIf (i >= appPrec) $ pprExp opPrec e1
                                              <+> pprExp appPrec e2
pprExp i (InfixE (Just e1) op (Just e2))
 = parensIf (i >= opPrec) $ pprExp opPrec e1
                        <+> pprInfixExp op
                        <+> pprExp opPrec e2
pprExp _ (InfixE me1 op me2) = parens $ pprMaybeExp noPrec me1
                                    <+> pprInfixExp op
                                    <+> pprMaybeExp noPrec me2
pprExp i (LamE ps e) = parensIf (i > noPrec) $ char '\\' <> hsep (map (pprPat appPrec) ps)
                                           <+> text "->" <+> ppr e
pprExp _ (TupE es) = parens $ sep $ punctuate comma $ map ppr es
-- Nesting in Cond is to avoid potential problems in do statments
pprExp i (CondE guard true false)
 = parensIf (i > noPrec) $ sep [text "if"   <+> ppr guard,
                       nest 1 $ text "then" <+> ppr true,
                       nest 1 $ text "else" <+> ppr false]
pprExp i (LetE ds e) = parensIf (i > noPrec) $ text "let" <+> ppr ds
                                            $$ text " in" <+> ppr e
pprExp i (CaseE e ms)
 = parensIf (i > noPrec) $ text "case" <+> ppr e <+> text "of"
                        $$ nest nestDepth (ppr ms)
pprExp i (DoE ss) = parensIf (i > noPrec) $ text "do" <+> ppr ss
pprExp _ (CompE []) = error "Can't happen: pprExp (CompExp [])"
-- This will probably break with fixity declarations - would need a ';'
pprExp _ (CompE ss) = text "[" <> ppr s
                  <+> text "|"
                  <+> (sep $ punctuate comma $ map ppr ss')
                   <> text "]"
    where s = last ss
          ss' = init ss
pprExp _ (ArithSeqE d) = ppr d
pprExp _ (ListE es) = brackets $ sep $ punctuate comma $ map ppr es
pprExp i (SigE e t) = parensIf (i > noPrec) $ ppr e <+> text "::" <+> ppr t
pprExp _ (RecConE nm fs) = ppr nm <> braces (pprFields fs)
pprExp _ (RecUpdE e fs) = pprExp appPrec e <> braces (pprFields fs)

pprFields :: [(Name,Exp)] -> Doc
pprFields = sep . punctuate comma . map (\(s,e) -> ppr s <+> equals <+> ppr e)

pprMaybeExp :: Precedence -> Maybe Exp -> Doc
pprMaybeExp _ Nothing = empty
pprMaybeExp i (Just e) = pprExp i e

------------------------------
instance Ppr Stmt where
    ppr (BindS p e) = ppr p <+> text "<-" <+> ppr e
    ppr (LetS ds) = text "let" <+> ppr ds
    ppr (NoBindS e) = ppr e
    ppr (ParS sss) = sep $ punctuate (text "|")
                         $ map (sep . punctuate comma . map ppr) sss

------------------------------
instance Ppr Match where
    ppr (Match p rhs ds) = ppr p <+> pprBody False rhs
                        $$ where_clause ds

------------------------------
pprBody :: Bool -> Body -> Doc
pprBody eq (GuardedB xs) = nest nestDepth $ vcat $ map do_guard xs
  where eqd = if eq then text "=" else text "->"
        do_guard (NormalG g, e) = text "|" <+> ppr g <+> eqd <+> ppr e
        do_guard (PatG ss, e) = text "|" <+> vcat (map ppr ss)
                             $$ nest nestDepth (eqd <+> ppr e)
pprBody eq (NormalB e) = (if eq then text "=" else text "->") <+> ppr e

------------------------------
pprLit :: Precedence -> Lit -> Doc
pprLit i (IntPrimL x)    = parensIf (i > noPrec && x < 0)
                                    (integer x <> char '#')
pprLit _ (WordPrimL x)    = integer x <> text "##"
pprLit i (FloatPrimL x)  = parensIf (i > noPrec && x < 0)
                                    (float (fromRational x) <> char '#')
pprLit i (DoublePrimL x) = parensIf (i > noPrec && x < 0)
                                    (double (fromRational x) <> text "##")
pprLit i (IntegerL x)    = parensIf (i > noPrec && x < 0) (integer x)
pprLit _ (CharL c)       = text (show c)
pprLit _ (StringL s)     = text (show s)
pprLit _ (StringPrimL s) = text (show s) <> char '#'
pprLit i (RationalL rat) = parensIf (i > noPrec) $ rational rat

------------------------------
instance Ppr Pat where
    ppr = pprPat noPrec

pprPat :: Precedence -> Pat -> Doc
pprPat i (LitP l)     = pprLit i l
pprPat _ (VarP v)     = pprName' Applied v
pprPat _ (TupP ps)    = parens $ sep $ punctuate comma $ map ppr ps
pprPat i (ConP s ps)  = parensIf (i >= appPrec) $ pprName' Applied s
                                              <+> sep (map (pprPat appPrec) ps)
pprPat i (InfixP p1 n p2)
                      = parensIf (i >= opPrec) (pprPat opPrec p1 <+>
                                                pprName' Infix n <+>
                                                pprPat opPrec p2)
pprPat i (TildeP p)   = parensIf (i > noPrec) $ char '~' <> pprPat appPrec p
pprPat i (BangP p)    = parensIf (i > noPrec) $ char '!' <> pprPat appPrec p
pprPat i (AsP v p)    = parensIf (i > noPrec) $ ppr v <> text "@"
                                                      <> pprPat appPrec p
pprPat _ WildP        = text "_"
pprPat _ (RecP nm fs)
 = parens $     ppr nm
            <+> braces (sep $ punctuate comma $
                        map (\(s,p) -> ppr s <+> equals <+> ppr p) fs)
pprPat _ (ListP ps) = brackets $ sep $ punctuate comma $ map ppr ps
pprPat i (SigP p t) = parensIf (i > noPrec) $ ppr p <+> text "::" <+> ppr t
pprPat _ (ViewP e p) = parens $ pprExp noPrec e <+> text "->" <+> pprPat noPrec p

------------------------------
instance Ppr Dec where
    ppr = ppr_dec True

ppr_dec :: Bool     -- declaration on the toplevel?
        -> Dec 
        -> Doc
ppr_dec _ (FunD f cs)   = vcat $ map (\c -> ppr f <+> ppr c) cs
ppr_dec _ (ValD p r ds) = ppr p <+> pprBody True r
                          $$ where_clause ds
ppr_dec _ (TySynD t xs rhs) 
  = ppr_tySyn empty t (hsep (map ppr xs)) rhs
ppr_dec _ (DataD ctxt t xs cs decs) 
  = ppr_data empty ctxt t (hsep (map ppr xs)) cs decs
ppr_dec _ (NewtypeD ctxt t xs c decs)
  = ppr_newtype empty ctxt t (sep (map ppr xs)) c decs
ppr_dec _  (ClassD ctxt c xs fds ds) 
  = text "class" <+> pprCxt ctxt <+> ppr c <+> hsep (map ppr xs) <+> ppr fds
    $$ where_clause ds
ppr_dec _ (InstanceD ctxt i ds) = text "instance" <+> pprCxt ctxt <+> ppr i
                                  $$ where_clause ds
ppr_dec _ (SigD f t) = ppr f <+> text "::" <+> ppr t
ppr_dec _ (ForeignD f) = ppr f
ppr_dec _ (PragmaD p) = ppr p
ppr_dec isTop (FamilyD flav tc tvs k) 
  = ppr flav <+> maybeFamily <+> ppr tc <+> hsep (map ppr tvs) <+> maybeKind
  where
    maybeFamily | isTop     = text "family"
                | otherwise = empty

    maybeKind | (Just k') <- k = text "::" <+> ppr k'
              | otherwise      = empty
ppr_dec isTop (DataInstD ctxt tc tys cs decs) 
  = ppr_data maybeInst ctxt tc (sep (map pprParendType tys)) cs decs
  where
    maybeInst | isTop     = text "instance"
              | otherwise = empty
ppr_dec isTop (NewtypeInstD ctxt tc tys c decs) 
  = ppr_newtype maybeInst ctxt tc (sep (map pprParendType tys)) c decs
  where
    maybeInst | isTop     = text "instance"
              | otherwise = empty
ppr_dec isTop (TySynInstD tc tys rhs) 
  = ppr_tySyn maybeInst tc (sep (map pprParendType tys)) rhs
  where
    maybeInst | isTop     = text "instance"
              | otherwise = empty

ppr_data :: Doc -> Cxt -> Name -> Doc -> [Con] -> [Name] -> Doc
ppr_data maybeInst ctxt t argsDoc cs decs
  = sep [text "data" <+> maybeInst
    	    <+> pprCxt ctxt
    	    <+> ppr t <+> argsDoc,
         nest nestDepth (sep (pref $ map ppr cs)),
         if null decs
           then empty
           else nest nestDepth
              $ text "deriving"
                <+> parens (hsep $ punctuate comma $ map ppr decs)]
  where 
    pref :: [Doc] -> [Doc]
    pref []     = []      -- No constructors; can't happen in H98
    pref (d:ds) = (char '=' <+> d):map (char '|' <+>) ds

ppr_newtype :: Doc -> Cxt -> Name -> Doc -> Con -> [Name] -> Doc
ppr_newtype maybeInst ctxt t argsDoc c decs
  = sep [text "newtype" <+> maybeInst
    	    <+> pprCxt ctxt
    	    <+> ppr t <+> argsDoc,
         nest 2 (char '=' <+> ppr c),
         if null decs
       	   then empty
       	   else nest nestDepth
       	        $ text "deriving"
       	          <+> parens (hsep $ punctuate comma $ map ppr decs)]

ppr_tySyn :: Doc -> Name -> Doc -> Type -> Doc
ppr_tySyn maybeInst t argsDoc rhs
  = text "type" <+> maybeInst <+> ppr t <+> argsDoc <+> text "=" <+> ppr rhs

------------------------------
instance Ppr FunDep where
    ppr (FunDep xs ys) = hsep (map ppr xs) <+> text "->" <+> hsep (map ppr ys)
    ppr_list [] = empty
    ppr_list xs = char '|' <+> sep (punctuate (text ", ") (map ppr xs))

------------------------------
instance Ppr FamFlavour where
    ppr DataFam = text "data"
    ppr TypeFam = text "type"

------------------------------
instance Ppr Foreign where
    ppr (ImportF callconv safety impent as typ)
       = text "foreign import"
     <+> showtextl callconv
     <+> showtextl safety
     <+> text (show impent)
     <+> ppr as
     <+> text "::" <+> ppr typ
    ppr (ExportF callconv expent as typ)
        = text "foreign export"
      <+> showtextl callconv
      <+> text (show expent)
      <+> ppr as
      <+> text "::" <+> ppr typ

------------------------------
instance Ppr Pragma where
    ppr (InlineP n (InlineSpec inline conlike activation))
       = text "{-#"
     <+> (if inline then text "INLINE" else text "NOINLINE")
     <+> (if conlike then text "CONLIKE" else empty)
     <+> ppr_activation activation 
     <+> ppr n
     <+> text "#-}"
    ppr (SpecialiseP n ty Nothing)
       = sep [ text "{-# SPECIALISE" 
             , ppr n <+> text "::"
             , ppr ty
             , text "#-}"
             ]
    ppr (SpecialiseP n ty (Just (InlineSpec inline _conlike activation)))
       = sep [ text "{-# SPECIALISE" <+> 
               (if inline then text "INLINE" else text "NOINLINE") <+>
               ppr_activation activation
             , ppr n <+> text "::"
             , ppr ty
             , text "#-}"
             ]
      where

ppr_activation :: Maybe (Bool, Int) -> Doc
ppr_activation (Just (beforeFrom, i))
  = brackets $ (if beforeFrom then empty else char '~') <+> int i
ppr_activation Nothing = empty

------------------------------
instance Ppr Clause where
    ppr (Clause ps rhs ds) = hsep (map (pprPat appPrec) ps) <+> pprBody True rhs
                             $$ where_clause ds

------------------------------
instance Ppr Con where
    ppr (NormalC c sts) = ppr c <+> sep (map pprStrictType sts)
    ppr (RecC c vsts)
        = ppr c <+> braces (sep (punctuate comma $ map pprVarStrictType vsts))
    ppr (InfixC st1 c st2) = pprStrictType st1
                         <+> pprName' Infix c
                         <+> pprStrictType st2
    ppr (ForallC ns ctxt con) = text "forall" <+> hsep (map ppr ns)
                            <+> char '.' <+> sep [pprCxt ctxt, ppr con]

------------------------------
pprVarStrictType :: (Name, Strict, Type) -> Doc
-- Slight infelicity: with print non-atomic type with parens
pprVarStrictType (v, str, t) = ppr v <+> text "::" <+> pprStrictType (str, t)

------------------------------
pprStrictType :: (Strict, Type) -> Doc
-- Prints with parens if not already atomic
pprStrictType (IsStrict, t) = char '!' <> pprParendType t
pprStrictType (NotStrict, t) = pprParendType t

------------------------------
pprParendType :: Type -> Doc
pprParendType (VarT v)   = ppr v
pprParendType (ConT c)   = ppr c
pprParendType (TupleT 0) = text "()"
pprParendType (TupleT n) = parens (hcat (replicate (n-1) comma))
pprParendType ArrowT     = parens (text "->")
pprParendType ListT      = text "[]"
pprParendType other      = parens (ppr other)

instance Ppr Type where
    ppr (ForallT tvars ctxt ty)
      = text "forall" <+> hsep (map ppr tvars) <+> text "."
                      <+> sep [pprCxt ctxt, ppr ty]
    ppr (SigT ty k) = ppr ty <+> text "::" <+> ppr k
    ppr ty          = pprTyApp (split ty)

pprTyApp :: (Type, [Type]) -> Doc
pprTyApp (ArrowT, [arg1,arg2]) = sep [pprFunArgType arg1 <+> text "->", ppr arg2]
pprTyApp (ListT, [arg]) = brackets (ppr arg)
pprTyApp (TupleT n, args)
 | length args == n = parens (sep (punctuate comma (map ppr args)))
pprTyApp (fun, args) = pprParendType fun <+> sep (map pprParendType args)

pprFunArgType :: Type -> Doc	-- Should really use a precedence argument
-- Everything except forall and (->) binds more tightly than (->)
pprFunArgType ty@(ForallT {})                 = parens (ppr ty)
pprFunArgType ty@((ArrowT `AppT` _) `AppT` _) = parens (ppr ty)
pprFunArgType ty@(SigT _ _)                   = parens (ppr ty)
pprFunArgType ty                              = ppr ty

split :: Type -> (Type, [Type])    -- Split into function and args
split t = go t []
    where go (AppT t1 t2) args = go t1 (t2:args)
          go ty           args = (ty, args)

------------------------------
instance Ppr TyVarBndr where
    ppr (PlainTV nm)    = ppr nm
    ppr (KindedTV nm k) = parens (ppr nm <+> text "::" <+> ppr k)

instance Ppr Kind where
    ppr StarK          = char '*'
    ppr (ArrowK k1 k2) = pprArrowArgKind k1 <+> text "->" <+> ppr k2

pprArrowArgKind :: Kind -> Doc
pprArrowArgKind k@(ArrowK _ _) = parens (ppr k)
pprArrowArgKind k              = ppr k

------------------------------
pprCxt :: Cxt -> Doc
pprCxt [] = empty
pprCxt [t] = ppr t <+> text "=>"
pprCxt ts = parens (sep $ punctuate comma $ map ppr ts) <+> text "=>"

------------------------------
instance Ppr Pred where
  ppr (ClassP cla tys) = ppr cla <+> sep (map pprParendType tys)
  ppr (EqualP ty1 ty2) = pprFunArgType ty1 <+> char '~' <+> pprFunArgType ty2

------------------------------
instance Ppr Range where
    ppr = brackets . pprRange
        where pprRange :: Range -> Doc
              pprRange (FromR e) = ppr e <> text ".."
              pprRange (FromThenR e1 e2) = ppr e1 <> text ","
                                        <> ppr e2 <> text ".."
              pprRange (FromToR e1 e2) = ppr e1 <> text ".." <> ppr e2
              pprRange (FromThenToR e1 e2 e3) = ppr e1 <> text ","
                                             <> ppr e2 <> text ".."
                                             <> ppr e3

------------------------------
where_clause :: [Dec] -> Doc
where_clause [] = empty
where_clause ds = nest nestDepth $ text "where" <+> vcat (map (ppr_dec False) ds)

showtextl :: Show a => a -> Doc
showtextl = text . map toLower . show

