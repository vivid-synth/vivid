{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE NoIncoherentInstances #-}

-- {-# LANGUAGE ConstraintKinds -}
-- {-# LANGUAGE GADTSyntax, NoMonoLocalBinds #-}
-- {-# LANGUAGE RankNTypes #-}

{-# LANGUAGE
     DataKinds
   , ExistentialQuantification
   , FlexibleContexts
   , FlexibleInstances
   , FunctionalDependencies
   , InstanceSigs
   , LambdaCase
   , MultiParamTypeClasses
   , PolyKinds
   , ScopedTypeVariables
   , StandaloneDeriving
   , TypeFamilies, NoMonoLocalBinds
   , TypeOperators
   , UndecidableInstances
   , ViewPatterns

   , DefaultSignatures
   , FlexibleContexts
   , StandaloneDeriving
   , TypeOperators
   #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

-- UndecidableInstances is needed for a nested type family instance ^

module Vivid.SynthDef.TypesafeArgs (

   -- * Type Set Operations
     SubsetBoolToBool -- (..)
   , ElemBoolToBool
   , SetInsert --(..)
   , SetEqual --(..)
   , SetUnion --(..)

   , IsSubsetOf --(..)
   , Bool_IsSubsetOf(..)
   , Subset -- (..)

   , IsElemOf
   , Bool_IsElemOf(..)
   , Elem

   -- * Type arguments
   , TagList(..)
   , VarList(..)
   , V
   , Vs(..)
      -- | This is aliased as 'V':
   , Variable(..)
   , I(..)
   , VarSet(..)
   , TypedVarList -- (..)
   , emptyVarSet
   , iToLiteralVar
   , iToVar
   , AddParams(..)

   -- * Other
--    , (?>) -- not exporting so it doesn't pollute namespace
   , toI
   , toF
   , GetSymbolVals(..)
   ) where

import Control.Arrow (first, second)
import GHC.Exts
import GHC.Generics
import GHC.TypeLits
import qualified Data.Map as Map
import Data.Proxy
import Data.Type.Bool
import System.Random -- For a Random instance

type family SubsetBoolToBool (a :: Bool_IsSubsetOf) :: Bool where
   SubsetBoolToBool 'True_IsSubsetOf = 'True
   SubsetBoolToBool 'False_IsSubsetOf = 'False


type family ElemBoolToBool (a :: Bool_IsElemOf) :: Bool where
   ElemBoolToBool 'True_IsElemOf = 'True
   ElemBoolToBool 'False_IsElemOf = 'False

-- | So for example if you have:
-- 
--   @
--   foo = sd (440 ::I "freq") $ do
--      s <- sinOsc (freq_ (A::A "freq!!!"))
--      out 0 [s,s]
--   @
--
--   It'll tell you that you have an invalid synthdef with the error:
-- 
--   @
--     Couldn't match type ‘'False_IsSubsetOf’ with ‘'True_IsSubsetOf’
--                             In the first argument of ‘sinOsc’, namely
--       ‘(freq_ (V :: A "freq!!!"))’
--   @
data Bool_IsSubsetOf
   = True_IsSubsetOf
   | False_IsSubsetOf

type family SetInsert (s :: x) (sSet :: [x]) :: [x] where
   SetInsert e (e ': xs) = (e ': xs)
   SetInsert e xs = e ': xs

type family SetUnion (a :: [x]) (b :: [x]) :: [x] where
   SetUnion '[] bs = bs
   SetUnion (anA ': as) bs = SetUnion as (SetInsert anA bs)

type family SetUnions (a :: [[x]]) :: [x] where
   SetUnions '[] = '[]
   SetUnions (x ': xs) = SetUnion x (SetUnions xs)

-- | >>  .  :kind! SetIntersection '[4,3,4,5] '[7,5,3]
--   >> SetIntersection '[4,3,4,5] '[7,5,3] :: [GHC.TypeLits.Nat]
--   >> = '[3, 5]
type family SetIntersection (a :: [x]) (b :: [x]) :: [x] where
   SetIntersection a b = SetIntersectionPrime a b b

type family SetIntersectionPrime (a :: [x]) (b :: [x]) (allB :: [x]) :: [x] where
   SetIntersectionPrime '[] b allB = '[]
   SetIntersectionPrime (a ': as) '[] allB =
      SetIntersectionPrime as allB allB
   SetIntersectionPrime (a ': as) (a ': bs) allB =
      a ': SetIntersectionPrime as allB allB
   SetIntersectionPrime (a ': as) (b ': bs) allB =
      SetIntersectionPrime (a ': as) bs allB

type family IsSubsetOf (a :: [x]) (b :: [x]) :: Bool_IsSubsetOf where
   IsSubsetOf a b = IsSubsetOfPrime a b b

type family IsSubsetOfPrime (a :: [x]) (b :: [x]) (fullBs :: [x]) :: Bool_IsSubsetOf where
   IsSubsetOfPrime '[] b fullBs = 'True_IsSubsetOf
   IsSubsetOfPrime (a ': as) (a ': bs) fullBs =
      IsSubsetOfPrime as fullBs fullBs
   IsSubsetOfPrime (a ': as) (b ': bs) fullBs =
      IsSubsetOfPrime (a ': as) bs fullBs
   IsSubsetOfPrime as '[] fullBs = 'False_IsSubsetOf

data Bool_IsElemOf
   = True_IsElemOf
   | False_IsElemOf

-- Can rewrite in terms of IsSubsetOf
-- (And have 'Elem' vs 'IsElemOf' -- first is the constraint)
type family IsElemOf (a :: x) (l :: [x]) :: Bool_IsElemOf where
   IsElemOf a '[] = 'False_IsElemOf
   IsElemOf a (a ': b) = 'True_IsElemOf
   IsElemOf a (notA ': b) = IsElemOf a b

-- | >>  >  :kind! SetEqual '["bye", "hi","bye","bye"] '["bye","hi","hi"]
--   >> SetEqual '["bye","hi","bye","bye"] '["bye","hi","hi"] :: Bool
--   >> = 'True
type family SetEqual (a :: [x]) (b :: [x]) :: Bool where
   SetEqual a b =
         SubsetBoolToBool (IsSubsetOf a b)
      && SubsetBoolToBool (IsSubsetOf b a)


data Variable (a :: Symbol) =
   KnownSymbol a => V


data Vs (a :: [Symbol]) = Vs

instance Show (Variable a) where
   show V = "(V::V " ++ show (symbolVal (Proxy::Proxy a)) ++ ")"

type V = Variable

{-
 >  getSymbolVals (Vs::Vs '["yes", "yes", "yall"])
["yes","yes","yall"]
-}

class GetSymbolVals x where
   getSymbolVals :: x -> [String]

instance GetSymbolVals (proxy '[]) where
   getSymbolVals _ = []

instance (KnownSymbol x, GetSymbolVals (Proxy (xs::[Symbol]))) =>
      GetSymbolVals (proxy (x ': xs)) where
   getSymbolVals _ =
      symbolVal (Proxy::Proxy x) : getSymbolVals (Proxy::Proxy xs)



data VarSet (s :: [Symbol]) = VarSet

instance (GetSymbolVals (VarSet xs)) => Show (VarSet xs) where
   show argSet =
      "(VarSet::VarSet "++show (getSymbolVals argSet)++")"

addVarToSet :: Variable sym -> VarSet syms -> VarSet (sym ': syms)
addVarToSet V VarSet = VarSet

emptyVarSet :: VarSet '[]
emptyVarSet = VarSet

data I (x :: Symbol) =
   KnownSymbol x => I Float

instance (KnownSymbol s) => Show (I s) where
   show (I f) =
      "(" ++ show f ++ "::I " ++ show (symbolVal (Proxy::Proxy s)) ++ ")"
deriving instance (KnownSymbol s) => Eq (I s)
deriving instance (KnownSymbol s) => Read (I s)
deriving instance (KnownSymbol s) => Ord (I s)

toI :: (Real n, KnownSymbol a) => n -> I a
toI = I . realToFrac

toF :: (Real n) => n -> Float
toF = realToFrac

instance (KnownSymbol s) => Num (I s) where
   fromInteger = I . fromInteger
   (+) (I a) (I b) = I (a + b)
   (-) (I a) (I b) = I (a - b)
   abs (I a) = I (abs a)
   (*) (I a) (I b) = I (a * b)
   signum (I a) = I (signum a)

{-
 .  map (* 20) [3..5] :: [I "note"]
[(60.0::I "note"),(80.0::I "note"),(100.0::I "note")]-}
-- etc

instance (KnownSymbol s) => Fractional (I s) where
   fromRational = I . fromRational
   (/) (I a) (I b) = I (a / b)

instance (KnownSymbol s) => Enum (I s) where
   fromEnum (I f) = fromEnum f
   toEnum = I . toEnum

instance (KnownSymbol s) => Real (I s) where
   toRational (I f) = toRational f

instance (KnownSymbol s) => RealFrac (I s) where
   properFraction (I f) = second I $ properFraction f
   truncate (I f) = truncate f
   round (I f) = round f
   ceiling (I f) = ceiling f
   floor (I f) = floor f

instance (KnownSymbol s) => Floating (I s) where
   pi = I pi
   exp (I i) = I (exp i)
   log (I i) = I (log i)
   sin (I i) = I (sin i)
   cos (I i) = I (cos i)
   asin (I i) = I (asin i)
   acos (I i) = I (acos i)
   atan (I i) = I (atan i)
   sinh (I i) = I (sinh i)
   cosh (I i) = I (cosh i)
   asinh (I i) = I (asinh i)
   acosh (I i) = I (acosh i)
   atanh (I i) = I (atanh i)

instance (KnownSymbol s) => RealFloat (I s) where
   floatRadix (I i) = floatRadix i
   floatDigits (I i) = floatDigits i
   floatRange (I i) = floatRange i
   decodeFloat (I i) = decodeFloat i
   encodeFloat a b = I $ encodeFloat a b
   isNaN (I i) = isNaN i
   isInfinite (I i) = isInfinite i
   isDenormalized (I i) = isDenormalized i
   isNegativeZero (I i) = isNegativeZero i
   isIEEE (I i) = isIEEE i

instance (KnownSymbol s) => Random (I s) where
   randomR :: RandomGen g => (I s, I s) -> g -> (I s, g)
   randomR (I rangeLo, I rangeHi) gen =
      first I $ randomR (rangeLo, rangeHi) gen

   random :: RandomGen g => g -> (I s, g)
   random gen = first I (random gen)

type TypedVarList (c :: [Symbol]) = ([(String, Float)], VarSet c)

iToLiteralVar :: KnownSymbol s => I s -> (String, Float)
iToLiteralVar i@(I f) = (symbolVal i, f)

iToVar :: KnownSymbol s => I s -> V s
iToVar _ = V

infixr ?>
(?>) :: (KnownSymbol a, VarList b) => I a -> b -> TypedVarList (a ': InnerVars b)
(?>) a b =
   let (re, name) = makeTypedVarList b
   in (iToLiteralVar a : re, addVarToSet {- (iToArg a) -} V name)

class VarList from where
   type InnerVars from :: [Symbol]
   makeTypedVarList :: from -> TypedVarList (InnerVars from)

{-
   default makeTypedVarList :: (Generic from) => from -> TypedVarList (InnerVars from)
   makeTypedVarList = undefined
-}

infixl `AddParams`

class TagList from where
   type AllTags from :: [Symbol]
   tagStrings :: from -> [String]

-- | Lets you combine sets of arguments. e.g.
-- 
--   > (1 ::I "foo", 2 ::I "bar") `AddParams` (3 ::I "baz")
-- 
--   means the same thing as
-- 
--   > (1 ::I "foo", 2 ::I "bar", 3 ::I "baz")
--   
-- 
--   This is left-biased, just like 'Map.union'
-- 
--   i.e. if you say:
-- 
--   > (99 ::I "same") `AddParams` (0 ::I "same")
-- 
--   It'll mean the same as
-- 
--   > (99 ::I "same")
data AddParams a b
   = (VarList a, VarList b) => AddParams a b

deriving instance (Show a, Show b) => Show (AddParams a b)

instance VarList (AddParams a b) where
   type InnerVars (AddParams a b) = SetUnion (InnerVars a) (InnerVars b)
   makeTypedVarList (a `AddParams` b) =
        let (Map.fromList -> fooA, _) = makeTypedVarList a
            (Map.fromList -> fooB, _) = makeTypedVarList b
            args = Map.toList $ fooA `Map.union` fooB
        in (args, VarSet :: VarSet (InnerVars (AddParams a b)))

{-
type family Subset (a :: [Symbol]) (b :: [Symbol]) :: Constraint where
   Subset a b = IsSubsetOf a b ~ 'True_IsSubsetOf

type family Elem (a :: Symbol) (b :: [Symbol]) :: Constraint where
   Elem a b = IsElemOf a b ~ 'True_IsElemOf
-}


type family Subset (as :: [Symbol]) (bs :: [Symbol]) :: Constraint where
   Subset '[] bs = ()
   Subset (a ': as) bs = (Elem a bs, Subset as bs)

type family Elem (a :: Symbol) (xs :: [Symbol]) :: Constraint where
   Elem a (a ': xs) = ()
   Elem a (x ': xs) = Elem a xs


-- INSTANCES:

-- | Wheeeeeeeeeeeeeeee!

instance VarList (TypedVarList a) where
   type InnerVars (TypedVarList a) = a
   makeTypedVarList = id

instance VarList () where
   type InnerVars () = '[]
   makeTypedVarList () = ([], emptyVarSet)

instance
   (KnownSymbol a) =>
   VarList (I a)
   where
      type InnerVars (I a) = '[a]
      makeTypedVarList x =
         ([iToLiteralVar (x)], VarSet::VarSet '[a])

instance
   (KnownSymbol a, KnownSymbol b) =>
   VarList (I a, I b)
   where
      type InnerVars (I a, I b) =
         '[a, b]
      makeTypedVarList (a,b) =
         a ?> b

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c) =>
   VarList (I a, I b, I c)
   where
      type InnerVars (I a, I b, I c) =
         '[a,b,c]
      makeTypedVarList (a,b,c) =
         a ?> b ?> c

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d) =>
   VarList (I a, I b, I c, I d)
   where
      type InnerVars (I a, I b, I c, I d) =
         '[a,b,c,d]
      makeTypedVarList (a,b,c,d) =
         a ?> b ?> c ?> d

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e) =>
   VarList
     (I a, I b, I c, I d, I e)
   where
      type InnerVars (I a, I b, I c, I d, I e) =
         '[a,b,c,d,e]
      makeTypedVarList (a,b,c,d,e) =
         a ?> b ?> c ?> d ?> e

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f) =
         '[a,b,c,d,e,f]
      makeTypedVarList (a,b,c,d,e,f) =
         a ?> b ?> c ?> d ?> e ?> f

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g) =
         '[a,b,c,d,e,f,g]
      makeTypedVarList (a,b,c,d,e,f,g) =
         a ?> b ?> c ?> d ?> e ?> f ?> g

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h) =
         '[a,b,c,d,e,f,g,h]
      makeTypedVarList (a,b,c,d,e,f,g,h) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i) =
         '[a,b,c,d,e,f,g,h,i]
      makeTypedVarList (a,b,c,d,e,f,g,h,i) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j) =
         '[a,b,c,d,e,f,g,h,i,j]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k) =
         '[a,b,c,d,e,f,g,h,i,j,k]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l) =
         '[a,b,c,d,e,f,g,h,i,j,k,l]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n

-- 15:
instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p, KnownSymbol q, KnownSymbol r, KnownSymbol s, KnownSymbol t
   ,KnownSymbol u, KnownSymbol v, KnownSymbol w, KnownSymbol x, KnownSymbol y
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y


instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e
   ,KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j
   ,KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p, KnownSymbol q, KnownSymbol r, KnownSymbol s, KnownSymbol t
   ,KnownSymbol u, KnownSymbol v, KnownSymbol w, KnownSymbol x, KnownSymbol y
   ,KnownSymbol z
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z




(##) :: (KnownSymbol a, TagList b) => V a -> b -> [String]
(##) (_::V a) b = symbolVal (Proxy::Proxy a) : tagStrings b

instance TagList () where
   type AllTags () = '[]
   tagStrings () = []

instance (KnownSymbol a) => TagList (V a) where
   type AllTags (V a) = '[a]
   tagStrings (_ :: V a) = [symbolVal (Proxy::Proxy a)]

instance (KnownSymbol a, KnownSymbol b) => TagList (V a, V b) where
   type AllTags (V a, V b) = '[a, b]
   tagStrings (a,b) = a ## b

-- Also works:
-- instance (KnownSymbol a , TagList (V b,V c)) => TagList (V a, V b, V c) where
instance (KnownSymbol a, KnownSymbol b, KnownSymbol c) => TagList (V a, V b, V c) where
   type AllTags (V a, V b, V c) = '[a, b, c]
   tagStrings (a,b,c) = a ## (b,c)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d) =>
      TagList (V a,V b,V c,V d) where
   type AllTags (V a,V b,V c,V d) =
      '[a,b,c,d]
   tagStrings (a,b,c,d) = a ## (b,c,d)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e) =>
      TagList (V a,V b,V c,V d,V e) where
   type AllTags (V a,V b,V c,V d,V e) =
      '[a,b,c,d,e]
   tagStrings (a,b,c,d,e) = a ## (b,c,d,e)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f
   ) =>
      TagList (V a,V b,V c,V d,V e,V f) where
   type AllTags (V a,V b,V c,V d,V e,V f) =
      '[a,b,c,d,e,f]
   tagStrings (a,b,c,d,e,f) = a ## (b,c,d,e,f)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g
   ) =>
      TagList (V a,V b,V c,V d,V e,V f,V g) where
   type AllTags (V a,V b,V c,V d,V e,V f,V g) =
      '[a,b,c,d,e,f,g]
   tagStrings (a,b,c,d,e,f,g) = a ## (b,c,d,e,f,g)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h
   ) =>
      TagList (V a,V b,V c,V d,V e,V f,V g,V h) where
   type AllTags (V a,V b,V c,V d,V e,V f,V g,V h) =
      '[a,b,c,d,e,f,g,h]
   tagStrings (a,b,c,d,e,f,g,h) = a ## (b,c,d,e,f,g,h)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i
   ) =>
      TagList (V a,V b,V c,V d,V e,V f,V g,V h,V i) where
   type AllTags (V a,V b,V c,V d,V e,V f,V g,V h,V i) =
      '[a,b,c,d,e,f,g,h,i]
   tagStrings (a,b,c,d,e,f,g,h,i) = a ## (b,c,d,e,f,g,h,i)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j
   ) =>
      TagList (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j) where
   type AllTags (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j) =
      '[a,b,c,d,e,f,g,h,i,j]
   tagStrings (a,b,c,d,e,f,g,h,i,j) = a ## (b,c,d,e,f,g,h,i,j)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k
   ) =>
      TagList (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k) where
   type AllTags (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k) =
      '[a,b,c,d,e,f,g,h,i,j,k]
   tagStrings (a,b,c,d,e,f,g,h,i,j,k) = a ## (b,c,d,e,f,g,h,i,j,k)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l
   ) =>
      TagList (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k,V l) where
   type AllTags (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k,V l) =
      '[a,b,c,d,e,f,g,h,i,j,k,l]
   tagStrings (a,b,c,d,e,f,g,h,i,j,k,l) = a ## (b,c,d,e,f,g,h,i,j,k,l)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m
   ) =>
      TagList (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k,V l,V m) where
   type AllTags (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k,V l,V m) =
      '[a,b,c,d,e,f,g,h,i,j,k,l,m]
   tagStrings (a,b,c,d,e,f,g,h,i,j,k,l,m) = a ## (b,c,d,e,f,g,h,i,j,k,l,m)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n
   ) =>
      TagList (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k,V l,V m,V n) where
   type AllTags (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k,V l,V m,V n) =
      '[a,b,c,d,e,f,g,h,i,j,k,l,m,n]
   tagStrings (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = a ## (b,c,d,e,f,g,h,i,j,k,l,m,n)

-- 15:
instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ) =>
      TagList (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k,V l,V m,V n,V o) where
   type AllTags (V a,V b,V c,V d,V e,V f,V g,V h,V i,V j,V k,V l,V m,V n,V o) =
      '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]
   tagStrings (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = a ## (b,c,d,e,f,g,h,i,j,k,l,m,n,o)
