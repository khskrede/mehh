\begin{code}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Integer
-- Copyright   :  (c) The University of Glasgow 1994-2008
-- License     :  see libraries/integer-gmp/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Integer' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define INT_MINBOUND (-2147483648#)
#define NEG_INT_MINBOUND (S# 2147483647# `plusInteger` S# 1#)
#elif SIZEOF_HSWORD == 8
#define INT_MINBOUND (-9223372036854775808#)
#define NEG_INT_MINBOUND (S# 9223372036854775807# `plusInteger` S# 1#)
#else
#error Unknown SIZEOF_HSWORD; can't define INT_MINBOUND and NEG_INT_MINBOUND
#endif

module GHC.Integer (
    Integer,
    smallInteger, wordToInteger, integerToWord, toInt#,
#if WORD_SIZE_IN_BITS < 64
    integerToWord64, word64ToInteger,
    integerToInt64, int64ToInteger,
#endif
    plusInteger, minusInteger, timesInteger, negateInteger,
    eqInteger, neqInteger, absInteger, signumInteger,
    leInteger, gtInteger, ltInteger, geInteger, compareInteger,
    divModInteger, quotRemInteger, quotInteger, remInteger,
    encodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,
    gcdInteger, lcmInteger,
    andInteger, orInteger, xorInteger, complementInteger,
    shiftLInteger, shiftRInteger,
    hashInteger,
 ) where

import GHC.Prim (
    -- Other types we use, convert from, or convert to
    Int#, Word#, Double#, Float#, ByteArray#,
    -- Conversions between those types
    int2Word#, int2Double#, int2Float#, word2Int#,
    -- Operations on Int# that we use for operations on S#
    quotInt#, remInt#, negateInt#,
    (==#), (/=#), (<=#), (>=#), (<#), (>#), (*#), (-#), (+#),
    mulIntMayOflo#, addIntC#, subIntC#,
    and#, or#, xor#
 )

import GHC.Integer.GMP.Internals (
    Integer(..),

    -- GMP-related primitives
    cmpInteger#, cmpIntegerInt#,
    plusInteger#, minusInteger#, timesInteger#,
    quotRemInteger#, quotInteger#, remInteger#, divModInteger#,
    gcdInteger#, gcdIntegerInt#, gcdInt#, divExactInteger#,
    decodeDouble#,
    int2Integer#, integer2Int#, word2Integer#, integer2Word#,
    andInteger#, orInteger#, xorInteger#, complementInteger#,
    mul2ExpInteger#, fdivQ2ExpInteger#,
#if WORD_SIZE_IN_BITS < 64
    int64ToInteger#,  integerToInt64#,
    word64ToInteger#, integerToWord64#,
#endif
 )

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64 (
            Int64#, Word64#,
            int64ToWord64#, intToInt64#,
            int64ToInt#, word64ToInt64#,
            geInt64#, leInt64#, leWord64#,
       )
#endif

import GHC.Bool
import GHC.Ordering

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway
\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ type}
%*                                                      *
%*********************************************************

Convenient boxed Integer PrimOps.

\begin{code}
{-# INLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i = S# i

{-# INLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = case word2Integer# w of (# s, d #) -> J# s d

{-# INLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (S# i) = int2Word# i
integerToWord (J# s d) = integer2Word# s d

#if WORD_SIZE_IN_BITS < 64
{-# INLINE integerToWord64 #-}
integerToWord64 :: Integer -> Word64#
integerToWord64 (S# i) = int64ToWord64# (intToInt64# i)
integerToWord64 (J# s d) = integerToWord64# s d

word64ToInteger :: Word64# -> Integer
word64ToInteger w = if w `leWord64#` int64ToWord64# (intToInt64# 0x7FFFFFFF#)
                    then S# (int64ToInt# (word64ToInt64# w))
                    else case word64ToInteger# w of
                         (# s, d #) -> J# s d

integerToInt64 :: Integer -> Int64#
integerToInt64 (S# i) = intToInt64# i
integerToInt64 (J# s d) = integerToInt64# s d

int64ToInteger :: Int64# -> Integer
int64ToInteger i = if ((i `leInt64#` intToInt64# 0x7FFFFFFF#) && 
                       (i `geInt64#` intToInt64# -0x80000000#))
                   then smallInteger (int64ToInt# i)
                   else case int64ToInteger# i of
                        (# s, d #) -> J# s d
    where -- XXX Move the (&&) definition below us?
          True  && x = x
          False && _ = False
#endif

toInt# :: Integer -> Int#
{-# NOINLINE toInt# #-}
{-# RULES "toInt#" forall i. toInt# (S# i) = i #-}
-- Don't inline toInt#, because it can't do much unless
-- it sees a (S# i), and inlining just creates fruitless
-- join points.  But we do need a RULE to get the constants
-- to work right:  1::Int had better optimise to (I# 1)!
toInt# (S# i)   = i
toInt# (J# s d) = integer2Int# s d

toBig :: Integer -> Integer
toBig (S# i)     = case int2Integer# i of { (# s, d #) -> J# s d }
toBig i@(J# _ _) = i
\end{code}


%*********************************************************
%*                                                      *
\subsection{Dividing @Integers@}
%*                                                      *
%*********************************************************

\begin{code}
-- XXX There's no good reason for us using unboxed tuples for the
-- results, but we don't have Data.Tuple available.

-- Note that we don't check for divide-by-zero here. That needs
-- to be done where it's used.
-- (we don't have error)

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger a@(S# INT_MINBOUND) b = quotRemInteger (toBig a) b
quotRemInteger (S# i) (S# j) = (# S# q, S# r #)
    where
      -- NB. don't inline these.  (# S# (i `quotInt#` j), ... #) means
      -- (# let q = i `quotInt#` j in S# q, ... #) which builds a
      -- useless thunk.  Placing the bindings here means they'll be
      -- evaluated strictly.
      !q = i `quotInt#` j
      !r = i `remInt#`  j
quotRemInteger i1@(J# _ _) i2@(S# _) = quotRemInteger i1 (toBig i2)
quotRemInteger i1@(S# _) i2@(J# _ _) = quotRemInteger (toBig i1) i2
quotRemInteger (J# s1 d1) (J# s2 d2)
  = case (quotRemInteger# s1 d1 s2 d2) of
          (# s3, d3, s4, d4 #)
            -> (# J# s3 d3, J# s4 d4 #)

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger a@(S# INT_MINBOUND) b = divModInteger (toBig a) b
divModInteger (S# i) (S# j) = (# S# d, S# m #)
    where
      -- NB. don't inline these.  See quotRemInteger above.
      !d = i `divInt#` j
      !m = i `modInt#` j

      -- XXX Copied from GHC.Base
      divInt# :: Int# -> Int# -> Int#
      x# `divInt#` y#
       =      if (x# ># 0#) && (y# <# 0#) then ((x# -# 1#) `quotInt#` y#) -# 1#
         else if (x# <# 0#) && (y# ># 0#) then ((x# +# 1#) `quotInt#` y#) -# 1#
         else x# `quotInt#` y#

      modInt# :: Int# -> Int# -> Int#
      x# `modInt#` y#
       = if ((x# ># 0#) && (y# <# 0#)) ||
            ((x# <# 0#) && (y# ># 0#))
         then if r# /=# 0# then r# +# y# else 0#
         else r#
          where !r# = x# `remInt#` y#

      (&&) :: Bool -> Bool -> Bool
      True  && x = x
      False && _ = False

      (||) :: Bool -> Bool -> Bool
      True  || _ = True
      False || x = x
divModInteger i1@(J# _ _) i2@(S# _) = divModInteger i1 (toBig i2)
divModInteger i1@(S# _) i2@(J# _ _) = divModInteger (toBig i1) i2
divModInteger (J# s1 d1) (J# s2 d2)
  = case (divModInteger# s1 d1 s2 d2) of
          (# s3, d3, s4, d4 #)
            -> (# J# s3 d3, J# s4 d4 #)

remInteger :: Integer -> Integer -> Integer
remInteger a@(S# INT_MINBOUND) b = remInteger (toBig a) b
remInteger (S# a) (S# b) = S# (remInt# a b)
{- Special case doesn't work, because a 1-element J# has the range
   -(2^32-1) -- 2^32-1, whereas S# has the range -2^31 -- (2^31-1)
remInteger ia@(S# a) (J# sb b)
  | sb ==# 1#  = S# (remInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (remInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | 0# <# sb   = ia
  | otherwise  = S# (0# -# a)
-}
remInteger ia@(S# _) ib@(J# _ _) = remInteger (toBig ia) ib
remInteger (J# sa a) (S# b)
  = case int2Integer# b of { (# sb, b' #) ->
    case remInteger# sa a sb b' of { (# sr, r #) ->
    S# (integer2Int# sr r) }}
remInteger (J# sa a) (J# sb b)
  = case remInteger# sa a sb b of (# sr, r #) -> J# sr r

quotInteger :: Integer -> Integer -> Integer
quotInteger a@(S# INT_MINBOUND) b = quotInteger (toBig a) b
quotInteger (S# a) (S# b) = S# (quotInt# a b)
{- Special case disabled, see remInteger above
quotInteger (S# a) (J# sb b)
  | sb ==# 1#  = S# (quotInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (quotInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | otherwise  = S# 0
-}
quotInteger ia@(S# _) ib@(J# _ _) = quotInteger (toBig ia) ib
quotInteger (J# sa a) (S# b)
  = case int2Integer# b of { (# sb, b' #) ->
    case quotInteger# sa a sb b' of (# sq, q #) -> J# sq q }
quotInteger (J# sa a) (J# sb b)
  = case quotInteger# sa a sb b of (# sg, g #) -> J# sg g
\end{code}



\begin{code}
-- We can't throw an error here, so it is up to our caller to
-- not call us with both arguments being 0.
gcdInteger :: Integer -> Integer -> Integer
-- SUP: Do we really need the first two cases?
gcdInteger a@(S# INT_MINBOUND) b = gcdInteger (toBig a) b
gcdInteger a b@(S# INT_MINBOUND) = gcdInteger a (toBig b)
gcdInteger (S# a) (S# b) = S# (gcdInt a b)
gcdInteger ia@(S# a)  ib@(J# sb b)
 =      if a  ==# 0# then absInteger ib
   else if sb ==# 0# then absInteger ia
   else                   S# (gcdIntegerInt# absSb b absA)
       where !absA  = if a  <# 0# then negateInt# a  else a
             !absSb = if sb <# 0# then negateInt# sb else sb
gcdInteger ia@(J# _ _) ib@(S# _) = gcdInteger ib ia
gcdInteger (J# sa a) (J# sb b)
  = case gcdInteger# sa a sb b of (# sg, g #) -> J# sg g

lcmInteger :: Integer -> Integer -> Integer
lcmInteger a b =      if a `eqInteger` S# 0# then S# 0#
                 else if b `eqInteger` S# 0# then S# 0#
                 else (divExact aa (gcdInteger aa ab)) `timesInteger` ab
  where aa = absInteger a
        ab = absInteger b

{-# RULES "gcdInteger/Int" forall a b.
            gcdInteger (S# a) (S# b) = S# (gcdInt a b)
  #-}
gcdInt :: Int# -> Int# -> Int#
gcdInt 0# y  = absInt y
gcdInt x  0# = absInt x
gcdInt x  y  = gcdInt# (absInt x) (absInt y)

absInt :: Int# -> Int#
absInt x = if x <# 0# then negateInt# x else x

divExact :: Integer -> Integer -> Integer
divExact a@(S# INT_MINBOUND) b = divExact (toBig a) b
divExact (S# a) (S# b) = S# (quotInt# a b)
divExact (S# a) (J# sb b)
  = S# (quotInt# a (integer2Int# sb b))
divExact (J# sa a) (S# b)
  = case int2Integer# b of
    (# sb, b' #) -> case divExactInteger# sa a sb b' of
                    (# sd, d #) -> J# sd d
divExact (J# sa a) (J# sb b)
  = case divExactInteger# sa a sb b of (# sd, d #) -> J# sd d
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Eq@, @Ord@}
%*                                                      *
%*********************************************************

\begin{code}
eqInteger :: Integer -> Integer -> Bool
eqInteger (S# i)     (S# j)     = i ==# j
eqInteger (S# i)     (J# s d)   = cmpIntegerInt# s d i ==# 0#
eqInteger (J# s d)   (S# i)     = cmpIntegerInt# s d i ==# 0#
eqInteger (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) ==# 0#

neqInteger :: Integer -> Integer -> Bool
neqInteger (S# i)     (S# j)     = i /=# j
neqInteger (S# i)     (J# s d)   = cmpIntegerInt# s d i /=# 0#
neqInteger (J# s d)   (S# i)     = cmpIntegerInt# s d i /=# 0#
neqInteger (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) /=# 0#

------------------------------------------------------------------------

leInteger :: Integer -> Integer -> Bool
leInteger (S# i)     (S# j)     = i <=# j
leInteger (J# s d)   (S# i)     = cmpIntegerInt# s d i <=# 0#
leInteger (S# i)     (J# s d)   = cmpIntegerInt# s d i >=# 0#
leInteger (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) <=# 0#

gtInteger :: Integer -> Integer -> Bool
gtInteger (S# i)     (S# j)     = i ># j
gtInteger (J# s d)   (S# i)     = cmpIntegerInt# s d i ># 0#
gtInteger (S# i)     (J# s d)   = cmpIntegerInt# s d i <# 0#
gtInteger (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) ># 0#

ltInteger :: Integer -> Integer -> Bool
ltInteger (S# i)     (S# j)     = i <# j
ltInteger (J# s d)   (S# i)     = cmpIntegerInt# s d i <# 0#
ltInteger (S# i)     (J# s d)   = cmpIntegerInt# s d i ># 0#
ltInteger (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) <# 0#

geInteger :: Integer -> Integer -> Bool
geInteger (S# i)     (S# j)     = i >=# j
geInteger (J# s d)   (S# i)     = cmpIntegerInt# s d i >=# 0#
geInteger (S# i)     (J# s d)   = cmpIntegerInt# s d i <=# 0#
geInteger (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) >=# 0#

compareInteger :: Integer -> Integer -> Ordering
compareInteger (S# i)  (S# j)
   =      if i ==# j then EQ
     else if i <=# j then LT
     else                 GT
compareInteger (J# s d) (S# i)
   = case cmpIntegerInt# s d i of { res# ->
     if res# <# 0# then LT else
     if res# ># 0# then GT else EQ
     }
compareInteger (S# i) (J# s d)
   = case cmpIntegerInt# s d i of { res# ->
     if res# ># 0# then LT else
     if res# <# 0# then GT else EQ
     }
compareInteger (J# s1 d1) (J# s2 d2)
   = case cmpInteger# s1 d1 s2 d2 of { res# ->
     if res# <# 0# then LT else
     if res# ># 0# then GT else EQ
     }
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Num@}
%*                                                      *
%*********************************************************

\begin{code}
{-# INLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger (S# INT_MINBOUND) = NEG_INT_MINBOUND
absInteger n@(S# i) = if i >=# 0# then n else S# (negateInt# i)
absInteger n@(J# s d) = if (s >=# 0#) then n else J# (negateInt# s) d

signumInteger :: Integer -> Integer
signumInteger (S# i) = if i <# 0# then S# -1#
                       else if i ==# 0# then S# 0#
                       else S# 1#
signumInteger (J# s d)
  = let
        !cmp = cmpIntegerInt# s d 0#
    in
    if      cmp >#  0# then S# 1#
    else if cmp ==# 0# then S# 0#
    else                    S# (negateInt# 1#)

plusInteger :: Integer -> Integer -> Integer
plusInteger i1@(S# i) i2@(S# j)  = case addIntC# i j of
                                   (# r, c #) ->
                                       if c ==# 0#
                                       then S# r
                                       else plusInteger (toBig i1) (toBig i2)
plusInteger i1@(J# _ _) i2@(S# _) = plusInteger i1 (toBig i2)
plusInteger i1@(S# _) i2@(J# _ _) = plusInteger (toBig i1) i2
plusInteger (J# s1 d1) (J# s2 d2) = case plusInteger# s1 d1 s2 d2 of
                                    (# s, d #) -> J# s d

minusInteger :: Integer -> Integer -> Integer
minusInteger i1@(S# i) i2@(S# j)   = case subIntC# i j of
                                     (# r, c #) ->
                                         if c ==# 0# then S# r
                                         else minusInteger (toBig i1)
                                                           (toBig i2)
minusInteger i1@(J# _ _) i2@(S# _) = minusInteger i1 (toBig i2)
minusInteger i1@(S# _) i2@(J# _ _) = minusInteger (toBig i1) i2
minusInteger (J# s1 d1) (J# s2 d2) = case minusInteger# s1 d1 s2 d2 of
                                     (# s, d #) -> J# s d

timesInteger :: Integer -> Integer -> Integer
timesInteger i1@(S# i) i2@(S# j)   = if mulIntMayOflo# i j ==# 0#
                                     then S# (i *# j)
                                     else timesInteger (toBig i1) (toBig i2)
timesInteger i1@(J# _ _) i2@(S# _) = timesInteger i1 (toBig i2)
timesInteger i1@(S# _) i2@(J# _ _) = timesInteger (toBig i1) i2
timesInteger (J# s1 d1) (J# s2 d2) = case timesInteger# s1 d1 s2 d2 of
                                     (# s, d #) -> J# s d

negateInteger :: Integer -> Integer
negateInteger (S# INT_MINBOUND) = NEG_INT_MINBOUND
negateInteger (S# i)            = S# (negateInt# i)
negateInteger (J# s d)          = J# (negateInt# s) d
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Integer@ stuff for Double@}
%*                                                      *
%*********************************************************

\begin{code}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger (S# i) j     = int_encodeFloat# i j
encodeFloatInteger (J# s# d#) e = encodeFloat# s# d# e

encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (S# i) j     = int_encodeDouble# i j
encodeDoubleInteger (J# s# d#) e = encodeDouble# s# d# e

decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d = case decodeDouble# d of
                        (# exp#, s#, d# #) -> (# J# s# d#, exp# #)

-- previous code: doubleFromInteger n = fromInteger n = encodeFloat n 0
-- doesn't work too well, because encodeFloat is defined in
-- terms of ccalls which can never be simplified away.  We
-- want simple literals like (fromInteger 3 :: Float) to turn
-- into (F# 3.0), hence the special case for S# here.

doubleFromInteger :: Integer -> Double#
doubleFromInteger (S# i#) = int2Double# i#
doubleFromInteger (J# s# d#) = encodeDouble# s# d# 0#

floatFromInteger :: Integer -> Float#
floatFromInteger (S# i#) = int2Float# i#
floatFromInteger (J# s# d#) = encodeFloat# s# d# 0#

foreign import ccall unsafe "integer_cbits_encodeFloat"
        encodeFloat# :: Int# -> ByteArray# -> Int# -> Float#
foreign import ccall unsafe "__int_encodeFloat"
        int_encodeFloat# :: Int# -> Int# -> Float#

foreign import ccall unsafe "integer_cbits_encodeDouble"
        encodeDouble# :: Int# -> ByteArray# -> Int# -> Double#
foreign import ccall unsafe "__int_encodeDouble"
        int_encodeDouble# :: Int# -> Int# -> Double#
\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ Bit definitions@}
%*                                                      *
%*********************************************************

We explicitly pattern match against J# and S# in order to produce
Core that doesn't have pattern matching errors, as that would
introduce a spurious dependency to base.

\begin{code}
andInteger :: Integer -> Integer -> Integer
(S# x) `andInteger` (S# y) = S# (word2Int# (int2Word# x `and#` int2Word# y))
x@(S# _) `andInteger` y@(J# _ _) = toBig x `andInteger` y
x@(J# _ _) `andInteger` y@(S# _) = x `andInteger` toBig y
(J# s1 d1) `andInteger` (J# s2 d2) =
     case andInteger# s1 d1 s2 d2 of
       (# s, d #) -> J# s d

orInteger :: Integer -> Integer -> Integer
(S# x) `orInteger` (S# y) = S# (word2Int# (int2Word# x `or#` int2Word# y))
x@(S# _) `orInteger` y@(J# _ _) = toBig x `orInteger` y
x@(J# _ _) `orInteger` y@(S# _) = x `orInteger` toBig y
(J# s1 d1) `orInteger` (J# s2 d2) =
     case orInteger# s1 d1 s2 d2 of
       (# s, d #) -> J# s d

xorInteger :: Integer -> Integer -> Integer
(S# x) `xorInteger` (S# y) = S# (word2Int# (int2Word# x `xor#` int2Word# y))
x@(S# _) `xorInteger` y@(J# _ _) = toBig x `xorInteger` y
x@(J# _ _) `xorInteger` y@(S# _) = x `xorInteger` toBig y
(J# s1 d1) `xorInteger` (J# s2 d2) =
     case xorInteger# s1 d1 s2 d2 of
       (# s, d #) -> J# s d

complementInteger :: Integer -> Integer
complementInteger (S# x)
    = S# (word2Int# (int2Word# x `xor#` int2Word# (0# -# 1#)))
complementInteger (J# s d)
    = case complementInteger# s d of (# s', d' #) -> J# s' d'

shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger j@(S# _) i = shiftLInteger (toBig j) i
shiftLInteger (J# s d) i = case mul2ExpInteger# s d i of
                           (# s', d' #) -> J# s' d'

shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger j@(S# _) i = shiftRInteger (toBig j) i
shiftRInteger (J# s d) i = case fdivQ2ExpInteger# s d i of
                           (# s', d' #) -> J# s' d'
\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ hashing@}
%*                                                      *
%*********************************************************

\begin{code}
-- This is used by hashUnique

-- | hashInteger returns the same value as 'fromIntegral', although in
-- unboxed form.  It might be a reasonable hash function for 'Integer', 
-- given a suitable distribution of 'Integer' values.

hashInteger :: Integer -> Int#
hashInteger = toInt#
                              
\end{code}

