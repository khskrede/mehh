{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- The -fno-warn-warnings-deprecations flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
-- for details

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Syntax
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Abstract syntax definitions for Template Haskell.
--
-----------------------------------------------------------------------------

module Language.Haskell.TH.Syntax(
	Quasi(..), Lift(..), liftString,

	Q, runQ, 
	report,	recover, reify,
	location, runIO,
        isClassInstance, classInstances,

	-- * Names
	Name(..), mkName, newName, nameBase, nameModule,
        showName, showName', NameIs(..),

	-- * The algebraic data types
	Dec(..), Exp(..), Con(..), Type(..), TyVarBndr(..), Kind(..),Cxt,
	Pred(..), Match(..),  Clause(..), Body(..), Guard(..), Stmt(..),
	Range(..), Lit(..), Pat(..), FieldExp, FieldPat, ClassInstance(..),
	Strict(..), Foreign(..), Callconv(..), Safety(..), Pragma(..),
	InlineSpec(..),	StrictType, VarStrictType, FunDep(..), FamFlavour(..),
	Info(..), Loc(..), CharPos,
	Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,

	-- * Internal functions
	returnQ, bindQ, sequenceQ,
	NameFlavour(..), NameSpace (..), 
	mkNameG_v, mkNameG_d, mkNameG_tc, Uniq, mkNameL, mkNameU,
 	tupleTypeName, tupleDataName,
	OccName, mkOccName, occString,
	ModName, mkModName, modString,
	PkgName, mkPkgName, pkgString
    ) where

import GHC.Base		( Int(..), Int#, (<#), (==#) )

import Language.Haskell.TH.Syntax.Internals
import Data.Data (Data(..), Typeable, mkConstr, mkDataType, constrIndex)
import qualified Data.Data as Data
import Data.IORef
import System.IO.Unsafe	( unsafePerformIO )
import Control.Monad (liftM)
import System.IO	( hPutStrLn, stderr )
import Data.Char        ( isAlpha )

-----------------------------------------------------
--
--		The Quasi class
--
-----------------------------------------------------

class (Monad m, Functor m) => Quasi m where
  qNewName :: String -> m Name
	-- ^ Fresh names

	-- Error reporting and recovery
  qReport  :: Bool -> String -> m ()	-- ^ Report an error (True) or warning (False)
					-- ...but carry on; use 'fail' to stop
  qRecover :: m a -- ^ the error handler
           -> m a -- ^ action which may fail
           -> m a		-- ^ Recover from the monadic 'fail'
 
	-- Inspect the type-checker's environment
  qReify :: Name -> m Info
  qClassInstances :: Name -> [Type] -> m [Name]
  		      -- Is (cls tys) an instance?
		      -- Returns list of matching witnesses

  qLocation :: m Loc

  qRunIO :: IO a -> m a
  -- ^ Input/output (dangerous)


-----------------------------------------------------
--	The IO instance of Quasi
-- 
--  This instance is used only when running a Q
--  computation in the IO monad, usually just to
--  print the result.  There is no interesting
--  type environment, so reification isn't going to
--  work.
--
-----------------------------------------------------

instance Quasi IO where
  qNewName s = do { n <- readIORef counter
                 ; writeIORef counter (n+1)
                 ; return (mkNameU s n) }

  qReport True  msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
  qReport False msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)

  qReify _            = badIO "reify"
  qClassInstances _ _ = badIO "classInstances"
  qLocation    	      = badIO "currentLocation"
  qRecover _ _ 	      = badIO "recover" -- Maybe we could fix this?

  qRunIO m = m
  
badIO :: String -> IO a
badIO op = do	{ qReport True ("Can't do `" ++ op ++ "' in the IO monad")
		; fail "Template Haskell failure" }

-- Global variable to generate unique symbols
counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)


-----------------------------------------------------
--
--		The Q monad
--
-----------------------------------------------------

newtype Q a = Q { unQ :: forall m. Quasi m => m a }

runQ :: Quasi m => Q a -> m a
runQ (Q m) = m

instance Monad Q where
  return x   = Q (return x)
  Q m >>= k  = Q (m >>= \x -> unQ (k x))
  Q m >> Q n = Q (m >> n)
  fail s     = report True s >> Q (fail "Q monad failure")

instance Functor Q where
  fmap f (Q x) = Q (fmap f x)

----------------------------------------------------
-- Packaged versions for the programmer, hiding the Quasi-ness
newName :: String -> Q Name
newName s = Q (qNewName s)

report  :: Bool -> String -> Q ()
report b s = Q (qReport b s)

recover :: Q a -- ^ recover with this one
        -> Q a -- ^ failing action
        -> Q a
recover (Q r) (Q m) = Q (qRecover r m)

-- | 'reify' looks up information about the 'Name'
reify :: Name -> Q Info
reify v = Q (qReify v)

-- | 'classInstances' looks up instaces of a class
classInstances :: Name -> [Type] -> Q [Name]
classInstances cls tys = Q (qClassInstances cls tys)

isClassInstance :: Name -> [Type] -> Q Bool
isClassInstance cls tys = do { dfuns <- classInstances cls tys
                             ; return (not (null dfuns)) }

-- | 'location' gives you the 'Location' at which this
-- computation is spliced.
location :: Q Loc
location = Q qLocation

-- |The 'runIO' function lets you run an I\/O computation in the 'Q' monad.
-- Take care: you are guaranteed the ordering of calls to 'runIO' within 
-- a single 'Q' computation, but not about the order in which splices are run.  
--
-- Note: for various murky reasons, stdout and stderr handles are not 
-- necesarily flushed when the  compiler finishes running, so you should
-- flush them yourself.
runIO :: IO a -> Q a
runIO m = Q (qRunIO m)

instance Quasi Q where
  qNewName  	  = newName
  qReport   	  = report
  qRecover  	  = recover 
  qReify    	  = reify
  qClassInstances = classInstances
  qLocation 	  = location
  qRunIO    	  = runIO


----------------------------------------------------
-- The following operations are used solely in DsMeta when desugaring brackets
-- They are not necessary for the user, who can use ordinary return and (>>=) etc

returnQ :: a -> Q a
returnQ = return

bindQ :: Q a -> (a -> Q b) -> Q b
bindQ = (>>=)

sequenceQ :: [Q a] -> Q [a]
sequenceQ = sequence


-----------------------------------------------------
--
--		The Lift class
--
-----------------------------------------------------

class Lift t where
  lift :: t -> Q Exp
  
instance Lift Integer where
  lift x = return (LitE (IntegerL x))

instance Lift Int where
  lift x= return (LitE (IntegerL (fromIntegral x)))

instance Lift Char where
  lift x = return (LitE (CharL x))

instance Lift Bool where
  lift True  = return (ConE trueName)
  lift False = return (ConE falseName)

instance Lift a => Lift (Maybe a) where
  lift Nothing  = return (ConE nothingName)
  lift (Just x) = liftM (ConE justName `AppE`) (lift x)

instance (Lift a, Lift b) => Lift (Either a b) where
  lift (Left x)  = liftM (ConE leftName  `AppE`) (lift x)
  lift (Right y) = liftM (ConE rightName `AppE`) (lift y)

instance Lift a => Lift [a] where
  lift xs = do { xs' <- mapM lift xs; return (ListE xs') }

liftString :: String -> Q Exp
-- Used in TcExpr to short-circuit the lifting for strings
liftString s = return (LitE (StringL s))

instance (Lift a, Lift b) => Lift (a, b) where
  lift (a, b)
    = liftM TupE $ sequence [lift a, lift b]

instance (Lift a, Lift b, Lift c) => Lift (a, b, c) where
  lift (a, b, c)
    = liftM TupE $ sequence [lift a, lift b, lift c]

instance (Lift a, Lift b, Lift c, Lift d) => Lift (a, b, c, d) where
  lift (a, b, c, d)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d]

instance (Lift a, Lift b, Lift c, Lift d, Lift e)
      => Lift (a, b, c, d, e) where
  lift (a, b, c, d, e)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f)
      => Lift (a, b, c, d, e, f) where
  lift (a, b, c, d, e, f)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g)
      => Lift (a, b, c, d, e, f, g) where
  lift (a, b, c, d, e, f, g)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f, lift g]

-- TH has a special form for literal strings,
-- which we should take advantage of.
-- NB: the lhs of the rule has no args, so that
--     the rule will apply to a 'lift' all on its own
--     which happens to be the way the type checker 
--     creates it.
{-# RULES "TH:liftString" lift = \s -> return (LitE (StringL s)) #-}


trueName, falseName :: Name
trueName  = mkNameG DataName "ghc-prim" "GHC.Bool" "True"
falseName = mkNameG DataName "ghc-prim" "GHC.Bool" "False"

nothingName, justName :: Name
nothingName = mkNameG DataName "base" "Data.Maybe" "Nothing"
justName    = mkNameG DataName "base" "Data.Maybe" "Just"

leftName, rightName :: Name
leftName  = mkNameG DataName "base" "Data.Either" "Left"
rightName = mkNameG DataName "base" "Data.Either" "Right"


-----------------------------------------------------
--		Names and uniques 
-----------------------------------------------------

mkModName :: String -> ModName
mkModName s = ModName s

modString :: ModName -> String
modString (ModName m) = m


mkPkgName :: String -> PkgName
mkPkgName s = PkgName s

pkgString :: PkgName -> String
pkgString (PkgName m) = m


-----------------------------------------------------
--		OccName
-----------------------------------------------------

mkOccName :: String -> OccName
mkOccName s = OccName s

occString :: OccName -> String
occString (OccName occ) = occ


-----------------------------------------------------
--		 Names
-----------------------------------------------------

-- |
-- For "global" names ('NameG') we need a totally unique name,
-- so we must include the name-space of the thing
--
-- For unique-numbered things ('NameU'), we've got a unique reference
-- anyway, so no need for name space
--
-- For dynamically bound thing ('NameS') we probably want them to
-- in a context-dependent way, so again we don't want the name
-- space.  For example:
--
-- > let v = mkName "T" in [| data $v = $v |]
--
-- Here we use the same Name for both type constructor and data constructor
--
--
-- NameL and NameG are bound *outside* the TH syntax tree
-- either globally (NameG) or locally (NameL). Ex:
--
-- > f x = $(h [| (map, x) |])
--
-- The 'map' will be a NameG, and 'x' wil be a NameL
--
-- These Names should never appear in a binding position in a TH syntax tree
data Name = Name OccName NameFlavour deriving (Typeable, Data)

data NameFlavour
  = NameS           -- ^ An unqualified name; dynamically bound
  | NameQ ModName   -- ^ A qualified name; dynamically bound

  | NameU Int#      -- ^ A unique local name


  | NameL Int#      -- ^ Local name bound outside of the TH AST
  | NameG NameSpace PkgName ModName -- ^ Global name bound outside of the TH AST:
                -- An original name (occurrences only, not binders)
                --
				-- Need the namespace too to be sure which 
				-- thing we are naming
  deriving ( Typeable )

-- |
-- Although the NameFlavour type is abstract, the Data instance is not. The reason for this
-- is that currently we use Data to serialize values in annotations, and in order for that to
-- work for Template Haskell names introduced via the 'x syntax we need gunfold on NameFlavour
-- to work. Bleh!
--
-- The long term solution to this is to use the binary package for annotation serialization and
-- then remove this instance. However, to do _that_ we need to wait on binary to become stable, since
-- boot libraries cannot be upgraded seperately from GHC itself.
--
-- This instance cannot be derived automatically due to bug #2701
instance Data NameFlavour where
     gfoldl _ z NameS          = z NameS
     gfoldl k z (NameQ mn)     = z NameQ `k` mn
     gfoldl k z (NameU i)      = z (\(I# i') -> NameU i') `k` (I# i)
     gfoldl k z (NameL i)      = z (\(I# i') -> NameL i') `k` (I# i)
     gfoldl k z (NameG ns p m) = z NameG `k` ns `k` p `k` m
     gunfold k z c = case constrIndex c of
         1 -> z NameS
         2 -> k $ z NameQ
         3 -> k $ z (\(I# i) -> NameU i)
         4 -> k $ z (\(I# i) -> NameL i)
         5 -> k $ k $ k $ z NameG
         _ -> error "gunfold: NameFlavour"
     toConstr NameS = con_NameS
     toConstr (NameQ _) = con_NameQ
     toConstr (NameU _) = con_NameU
     toConstr (NameL _) = con_NameL
     toConstr (NameG _ _ _) = con_NameG
     dataTypeOf _ = ty_NameFlavour

con_NameS, con_NameQ, con_NameU, con_NameL, con_NameG :: Data.Constr
con_NameS = mkConstr ty_NameFlavour "NameS" [] Data.Prefix
con_NameQ = mkConstr ty_NameFlavour "NameQ" [] Data.Prefix
con_NameU = mkConstr ty_NameFlavour "NameU" [] Data.Prefix
con_NameL = mkConstr ty_NameFlavour "NameL" [] Data.Prefix
con_NameG = mkConstr ty_NameFlavour "NameG" [] Data.Prefix

ty_NameFlavour :: Data.DataType
ty_NameFlavour = mkDataType "Language.Haskell.TH.Syntax.NameFlavour"
                            [con_NameS, con_NameQ, con_NameU,
                             con_NameL, con_NameG]

data NameSpace = VarName	-- ^ Variables
	       | DataName	-- ^ Data constructors 
	       | TcClsName	-- ^ Type constructors and classes; Haskell has them
				-- in the same name space for now.
	       deriving( Eq, Ord, Data, Typeable )

type Uniq = Int

-- | Base, unqualified name.
nameBase :: Name -> String
nameBase (Name occ _) = occString occ

nameModule :: Name -> Maybe String
nameModule (Name _ (NameQ m))     = Just (modString m)
nameModule (Name _ (NameG _ _ m)) = Just (modString m)
nameModule _                      = Nothing

mkName :: String -> Name
-- ^ The string can have a '.', thus "Foo.baz",
-- giving a dynamically-bound qualified name,
-- in which case we want to generate a NameQ
--
-- Parse the string to see if it has a "." in it
-- so we know whether to generate a qualified or unqualified name
-- It's a bit tricky because we need to parse 
--
-- > Foo.Baz.x   as    Qual Foo.Baz x
--
-- So we parse it from back to front
mkName str
  = split [] (reverse str)
  where
    split occ []        = Name (mkOccName occ) NameS
    split occ ('.':rev)	| not (null occ), 
			  not (null rev), head rev /= '.'
			= Name (mkOccName occ) (NameQ (mkModName (reverse rev)))
	-- The 'not (null occ)' guard ensures that
	-- 	mkName "&." = Name "&." NameS
	-- The 'rev' guards ensure that
	--	mkName ".&" = Name ".&" NameS
	--	mkName "Data.Bits..&" = Name ".&" (NameQ "Data.Bits")
	-- This rather bizarre case actually happened; (.&.) is in Data.Bits
    split occ (c:rev)   = split (c:occ) rev

-- | Only used internally
mkNameU :: String -> Uniq -> Name
mkNameU s (I# u) = Name (mkOccName s) (NameU u)

-- | Only used internally
mkNameL :: String -> Uniq -> Name
mkNameL s (I# u) = Name (mkOccName s) (NameL u)

-- | Used for 'x etc, but not available to the programmer
mkNameG :: NameSpace -> String -> String -> String -> Name
mkNameG ns pkg modu occ
  = Name (mkOccName occ) (NameG ns (mkPkgName pkg) (mkModName modu))

mkNameG_v, mkNameG_tc, mkNameG_d :: String -> String -> String -> Name
mkNameG_v  = mkNameG VarName
mkNameG_tc = mkNameG TcClsName
mkNameG_d  = mkNameG DataName

instance Eq Name where
  v1 == v2 = cmpEq (v1 `compare` v2)

instance Ord Name where
  (Name o1 f1) `compare` (Name o2 f2) = (f1 `compare` f2)   `thenCmp`
				        (o1 `compare` o2)

instance Eq NameFlavour where
  f1 == f2 = cmpEq (f1 `compare` f2)

instance Ord NameFlavour where
	-- NameS < NameQ < NameU < NameL < NameG
  NameS `compare` NameS = EQ
  NameS `compare` _     = LT

  (NameQ _)  `compare` NameS      = GT
  (NameQ m1) `compare` (NameQ m2) = m1 `compare` m2
  (NameQ _)  `compare` _          = LT

  (NameU _)  `compare` NameS      = GT
  (NameU _)  `compare` (NameQ _)  = GT
  (NameU u1) `compare` (NameU u2) | u1  <# u2 = LT
				  | u1 ==# u2 = EQ
				  | otherwise = GT
  (NameU _)  `compare` _     = LT

  (NameL _)  `compare` NameS      = GT
  (NameL _)  `compare` (NameQ _)  = GT
  (NameL _)  `compare` (NameU _)  = GT
  (NameL u1) `compare` (NameL u2) | u1  <# u2 = LT
				  | u1 ==# u2 = EQ
				  | otherwise = GT
  (NameL _)  `compare` _          = LT

  (NameG ns1 p1 m1) `compare` (NameG ns2 p2 m2) = (ns1 `compare` ns2) `thenCmp`
                                            (p1 `compare` p2) `thenCmp`
					    (m1 `compare` m2) 
  (NameG _ _ _)    `compare` _ = GT

data NameIs = Alone | Applied | Infix

showName :: Name -> String
showName = showName' Alone

showName' :: NameIs -> Name -> String
showName' ni nm
 = case ni of
       Alone        -> nms
       Applied
        | pnam      -> nms
        | otherwise -> "(" ++ nms ++ ")"
       Infix
        | pnam      -> "`" ++ nms ++ "`"
        | otherwise -> nms
    where
	-- For now, we make the NameQ and NameG print the same, even though
	-- NameQ is a qualified name (so what it means depends on what the
	-- current scope is), and NameG is an original name (so its meaning
	-- should be independent of what's in scope.
	-- We may well want to distinguish them in the end.
	-- Ditto NameU and NameL
        nms = case nm of
                    Name occ NameS         -> occString occ
                    Name occ (NameQ m)     -> modString m ++ "." ++ occString occ
                    Name occ (NameG _ _ m) -> modString m ++ "." ++ occString occ
                    Name occ (NameU u)     -> occString occ ++ "_" ++ show (I# u)
                    Name occ (NameL u)     -> occString occ ++ "_" ++ show (I# u)

        pnam = classify nms

        -- True if we are function style, e.g. f, [], (,)
        -- False if we are operator style, e.g. +, :+
        classify "" = False -- shouldn't happen; . operator is handled below
        classify (x:xs) | isAlpha x || (x `elem` "_[]()") =
                            case dropWhile (/='.') xs of
                                  (_:xs') -> classify xs'
                                  []      -> True
                        | otherwise = False

instance Show Name where
  show = showName

-- 	Tuple data and type constructors
tupleDataName :: Int -> Name    -- ^ Data constructor
tupleTypeName :: Int -> Name    -- ^ Type constructor

tupleDataName 0 = mk_tup_name 0 DataName 
tupleDataName 1 = error "tupleDataName 1"
tupleDataName n = mk_tup_name (n-1) DataName 

tupleTypeName 0 = mk_tup_name 0 TcClsName 
tupleTypeName 1 = error "tupleTypeName 1"
tupleTypeName n = mk_tup_name (n-1) TcClsName 

mk_tup_name :: Int -> NameSpace -> Name
mk_tup_name n_commas space
  = Name occ (NameG space (mkPkgName "ghc-prim") tup_mod)
  where
    occ = mkOccName ('(' : replicate n_commas ',' ++ ")")
    -- XXX Should it be GHC.Unit for 0 commas?
    tup_mod = mkModName "GHC.Tuple"



-----------------------------------------------------
--		Locations
-----------------------------------------------------

data Loc
  = Loc { loc_filename :: String
	, loc_package  :: String
	, loc_module   :: String
	, loc_start    :: CharPos
	, loc_end      :: CharPos }

type CharPos = (Int, Int)	-- Line and character position


-----------------------------------------------------
--
--	The Info returned by reification
--
-----------------------------------------------------

-- | Obtained from 'reify' in the 'Q' Monad.
data Info
  = -- | A class is reified to its declaration 
    --   and a list of its instances
    ClassI 
        Dec             -- Declaration of the class
        [ClassInstance]	-- The instances of that class

  | ClassOpI
	Name	-- The class op itself
	Type 	-- Type of the class-op (fully polymoprhic)
	Name 	-- Name of the parent class
	Fixity

  | TyConI Dec

  | PrimTyConI 	-- Ones that can't be expressed with a data type 
		-- decl, such as (->), Int#
	Name 
	Int	-- Arity
	Bool	-- False => lifted type; True => unlifted

  | DataConI 
	Name	-- The data con itself
	Type 	-- Type of the constructor (fully polymorphic)
	Name 	-- Name of the parent TyCon
	Fixity

  | VarI 
	Name	-- The variable itself
	Type 
	(Maybe Dec)	-- Nothing for lambda-bound variables, and 
			-- for anything else TH can't figure out
			-- E.g. [| let x = 1 in $(do { d <- reify 'x; .. }) |]
	Fixity

  | TyVarI 	-- Scoped type variable
	Name
	Type	-- What it is bound to
  deriving( Show, Data, Typeable )

-- | 'ClassInstance' desribes a single instance of a class
data ClassInstance 
  = ClassInstance {
      ci_dfun :: Name,	  -- The witness
      ci_tvs  :: [TyVarBndr], 
      ci_cxt  :: Cxt,
      ci_cls  :: Name,  
      ci_tys  :: [Type]
    } deriving( Show, Data, Typeable )

data Fixity          = Fixity Int FixityDirection
    deriving( Eq, Show, Data, Typeable )
data FixityDirection = InfixL | InfixR | InfixN
    deriving( Eq, Show, Data, Typeable )

maxPrecedence :: Int
maxPrecedence = (9::Int)

defaultFixity :: Fixity
defaultFixity = Fixity maxPrecedence InfixL


-----------------------------------------------------
--
--	The main syntax data types
--
-----------------------------------------------------

data Lit = CharL Char 
         | StringL String 
         | IntegerL Integer     -- ^ Used for overloaded and non-overloaded
                                -- literals. We don't have a good way to
                                -- represent non-overloaded literals at
                                -- the moment. Maybe that doesn't matter?
         | RationalL Rational   -- Ditto
         | IntPrimL Integer
         | WordPrimL Integer
         | FloatPrimL Rational
         | DoublePrimL Rational
         | StringPrimL String	-- ^ A primitive C-style string, type Addr#
    deriving( Show, Eq, Data, Typeable )

    -- We could add Int, Float, Double etc, as we do in HsLit, 
    -- but that could complicate the
    -- suppposedly-simple TH.Syntax literal type

-- | Pattern in Haskell given in @{}@
data Pat 
  = LitP Lit                      -- ^ @{ 5 or 'c' }@
  | VarP Name                     -- ^ @{ x }@
  | TupP [Pat]                    -- ^ @{ (p1,p2) }@
  | ConP Name [Pat]               -- ^ @data T1 = C1 t1 t2; {C1 p1 p1} = e@
  | InfixP Pat Name Pat           -- ^ @foo ({x :+ y}) = e@
  | TildeP Pat                    -- ^ @{ ~p }@
  | BangP Pat                     -- ^ @{ !p }@
  | AsP Name Pat                  -- ^ @{ x \@ p }@
  | WildP                         -- ^ @{ _ }@
  | RecP Name [FieldPat]          -- ^ @f (Pt { pointx = x }) = g x@
  | ListP [ Pat ]                 -- ^ @{ [1,2,3] }@
  | SigP Pat Type                 -- ^ @{ p :: t }@
  | ViewP Exp Pat                 -- ^ @{ e -> p }@
  deriving( Show, Eq, Data, Typeable )

type FieldPat = (Name,Pat)

data Match = Match Pat Body [Dec] -- ^ @case e of { pat -> body where decs }@
    deriving( Show, Eq, Data, Typeable )
data Clause = Clause [Pat] Body [Dec]
                                  -- ^ @f { p1 p2 = body where decs }@
    deriving( Show, Eq, Data, Typeable )
 
-- | The 'CompE' constructor represents a list comprehension, and 
-- takes a ['Stmt'].  The result expression of the comprehension is
-- the *last* of these, and should be a 'NoBindS'.
--
-- E.g. translation:
--
-- > [ f x | x <- xs ]
--
-- > CompE [BindS (VarP x) (VarE xs), NoBindS (AppE (VarE f) (VarE x))]
data Exp 
  = VarE Name                          -- ^ @{ x }@
  | ConE Name                          -- ^ @data T1 = C1 t1 t2; p = {C1} e1 e2  @
  | LitE Lit                           -- ^ @{ 5 or 'c'}@
  | AppE Exp Exp                       -- ^ @{ f x }@

  | InfixE (Maybe Exp) Exp (Maybe Exp) -- ^ @{x + y} or {(x+)} or {(+ x)} or {(+)}@
    --
    -- It's a bit gruesome to use an Exp as the
    -- operator, but how else can we distinguish
    -- constructors from non-constructors?
    -- Maybe there should be a var-or-con type?
    -- Or maybe we should leave it to the String itself?

  | LamE [Pat] Exp                     -- ^ @{ \ p1 p2 -> e }@
  | TupE [Exp]                         -- ^ @{ (e1,e2) }  @
  | CondE Exp Exp Exp                  -- ^ @{ if e1 then e2 else e3 }@
  | LetE [Dec] Exp                     -- ^ @{ let x=e1;   y=e2 in e3 }@
  | CaseE Exp [Match]                  -- ^ @{ case e of m1; m2 }@
  | DoE [Stmt]                         -- ^ @{ do { p <- e1; e2 }  }@
  | CompE [Stmt]                       -- ^ @{ [ (x,y) | x <- xs, y <- ys ] }@
  | ArithSeqE Range                    -- ^ @{ [ 1 ,2 .. 10 ] }@
  | ListE [ Exp ]                      -- ^ @{ [1,2,3] }@
  | SigE Exp Type                      -- ^ @{ e :: t }@
  | RecConE Name [FieldExp]            -- ^ @{ T { x = y, z = w } }@
  | RecUpdE Exp [FieldExp]             -- ^ @{ (f x) { z = w } }@
  deriving( Show, Eq, Data, Typeable )

type FieldExp = (Name,Exp)

-- Omitted: implicit parameters

data Body
  = GuardedB [(Guard,Exp)]   -- ^ @f p { | e1 = e2 | e3 = e4 } where ds@
  | NormalB Exp              -- ^ @f p { = e } where ds@
  deriving( Show, Eq, Data, Typeable )

data Guard
  = NormalG Exp
  | PatG [Stmt]
  deriving( Show, Eq, Data, Typeable )

data Stmt
  = BindS Pat Exp
  | LetS [ Dec ]
  | NoBindS Exp
  | ParS [[Stmt]]
  deriving( Show, Eq, Data, Typeable )

data Range = FromR Exp | FromThenR Exp Exp
           | FromToR Exp Exp | FromThenToR Exp Exp Exp
          deriving( Show, Eq, Data, Typeable )
  
data Dec 
  = FunD Name [Clause]            -- ^ @{ f p1 p2 = b where decs }@
  | ValD Pat Body [Dec]           -- ^ @{ p = b where decs }@
  | DataD Cxt Name [TyVarBndr] 
         [Con] [Name]             -- ^ @{ data Cxt x => T x = A x | B (T x)
                                  --       deriving (Z,W)}@
  | NewtypeD Cxt Name [TyVarBndr] 
         Con [Name]               -- ^ @{ newtype Cxt x => T x = A (B x)
                                  --       deriving (Z,W)}@
  | TySynD Name [TyVarBndr] Type  -- ^ @{ type T x = (x,x) }@
  | ClassD Cxt Name [TyVarBndr] 
         [FunDep] [Dec]           -- ^ @{ class Eq a => Ord a where ds }@
  | InstanceD Cxt Type [Dec]      -- ^ @{ instance Show w => Show [w]
                                  --       where ds }@
  | SigD Name Type                -- ^ @{ length :: [a] -> Int }@
  | ForeignD Foreign

  -- | pragmas
  | PragmaD Pragma                -- ^ @{ {-# INLINE [1] foo #-} }@

  -- | type families (may also appear in [Dec] of 'ClassD' and 'InstanceD')
  | FamilyD FamFlavour Name 
         [TyVarBndr] (Maybe Kind) -- ^ @{ type family T a b c :: * }@
                                 
  | DataInstD Cxt Name [Type]
         [Con] [Name]             -- ^ @{ data instance Cxt x => T [x] = A x 
                                  --                                | B (T x)
                                  --       deriving (Z,W)}@
  | NewtypeInstD Cxt Name [Type]
         Con [Name]               -- ^ @{ newtype instance Cxt x => T [x] = A (B x)
                                  --       deriving (Z,W)}@
  | TySynInstD Name [Type] Type   -- ^ @{ type instance T (Maybe x) = (x,x) }@
  deriving( Show, Eq, Data, Typeable )

data FunDep = FunDep [Name] [Name]
  deriving( Show, Eq, Data, Typeable )

data FamFlavour = TypeFam | DataFam
  deriving( Show, Eq, Data, Typeable )

data Foreign = ImportF Callconv Safety String Name Type
             | ExportF Callconv        String Name Type
         deriving( Show, Eq, Data, Typeable )

data Callconv = CCall | StdCall
          deriving( Show, Eq, Data, Typeable )

data Safety = Unsafe | Safe | Threadsafe
        deriving( Show, Eq, Data, Typeable )

data Pragma = InlineP     Name InlineSpec
            | SpecialiseP Name Type (Maybe InlineSpec)
        deriving( Show, Eq, Data, Typeable )

data InlineSpec 
  = InlineSpec Bool                 -- False: no inline; True: inline 
               Bool                 -- False: fun-like; True: constructor-like
               (Maybe (Bool, Int))  -- False: before phase; True: from phase
  deriving( Show, Eq, Data, Typeable )

type Cxt = [Pred]                 -- ^ @(Eq a, Ord b)@

data Pred = ClassP Name [Type]    -- ^ @Eq (Int, a)@
          | EqualP Type Type      -- ^ @F a ~ Bool@
          deriving( Show, Eq, Data, Typeable )

data Strict = IsStrict | NotStrict
         deriving( Show, Eq, Data, Typeable )

data Con = NormalC Name [StrictType]          -- ^ @C Int a@
         | RecC Name [VarStrictType]          -- ^ @C { v :: Int, w :: a }@
         | InfixC StrictType Name StrictType  -- ^ @Int :+ a@
         | ForallC [TyVarBndr] Cxt Con        -- ^ @forall a. Eq a => C [a]@
         deriving( Show, Eq, Data, Typeable )

type StrictType = (Strict, Type)
type VarStrictType = (Name, Strict, Type)

data Type = ForallT [TyVarBndr] Cxt Type  -- ^ @forall <vars>. <ctxt> -> <type>@
          | VarT Name                     -- ^ @a@
          | ConT Name                     -- ^ @T@
          | TupleT Int                    -- ^ @(,), (,,), etc.@
          | ArrowT                        -- ^ @->@
          | ListT                         -- ^ @[]@
          | AppT Type Type                -- ^ @T a b@
          | SigT Type Kind                -- ^ @t :: k@
      deriving( Show, Eq, Data, Typeable )

data TyVarBndr = PlainTV  Name            -- ^ @a@
               | KindedTV Name Kind       -- ^ @(a :: k)@
      deriving( Show, Eq, Data, Typeable )

data Kind = StarK                         -- ^ @'*'@
          | ArrowK Kind Kind              -- ^ @k1 -> k2@
      deriving( Show, Eq, Data, Typeable )

-----------------------------------------------------
--		Internal helper functions
-----------------------------------------------------

cmpEq :: Ordering -> Bool
cmpEq EQ = True
cmpEq _  = False

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _  = o1

