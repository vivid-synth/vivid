-- The `concat <$> sequence _` is to not run afoul of '-fconstraint-solver-iterations' -- temp?

{-# OPTIONS_HADDOCK show-extensions #-}

-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs, NoMonoLocalBinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
-- Needed for nested type family application:
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


--{-# LANGUAGE RankNTypes #-}

module Vivid.SynthDef.FromUA (
     FromUA(..)
   , fromUAWithDefaults
   , uaArgVal
   , uaArgValWDefault
   , DefaultArgs(..)
   , OverwritingArgs(..)
   , UA(..)
   , NoDefaults(..)
   , none
   , Args
   , SDBody

   , AllEqual
   ) where

-- import Vivid.SynthDef
import Vivid.SynthDef.ToSig
import Vivid.SynthDef.Types
-- import Vivid.SynthDef.TypesafeArgs

import qualified Data.List as L
import qualified Data.Map as Map
import Data.Monoid
import Data.Proxy
import GHC.Exts
import GHC.TypeLits

type SDBody a = SDBody' (SDBodyArgs a)

class FromUA (a :: *) where
   type UAsArgs a :: [Symbol]
   type SDBodyArgs a :: [Symbol]
   fromUA :: a -> SDBody a [(String, Signal)]

fromUAWithDefaults :: (
     FromUA a, FromUA b
   , SDBodyArgs a ~ SDBodyArgs b
   ) => DefaultArgs a -> OverwritingArgs b -> SDBody a [(String, Signal)]
fromUAWithDefaults (DefaultArgs defaultArgs) (OverwritingArgs overwritingArgs) = do
   defaultArgs' <- fromUA defaultArgs
   overwritingArgs' <- fromUA overwritingArgs
   return . Map.toList $
      Map.unionWith
         (\_defaultArg overwritingArg -> overwritingArg)
         (Map.fromList defaultArgs')
         (Map.fromList overwritingArgs')

-- Newtypes so we don't accidentally flip argument order:
newtype DefaultArgs a = DefaultArgs a
newtype OverwritingArgs a = OverwritingArgs a

data NoDefaults (args :: [Symbol])
   = NoDefaults
 deriving (Show, Eq, Ord, Read)

none :: NoDefaults args
none = NoDefaults

uaArgVal :: (FromUA as, Elem aToLookUp (UAsArgs as), KnownSymbol aToLookUp) => as -> proxy aToLookUp -> SDBody as Signal
uaArgVal uaArgs proxy = do
   allSigs <- fromUA uaArgs
   return $ case L.lookup (symbolVal proxy) (allSigs::[(String,Signal)]) of
      Just x -> x
      Nothing -> error $ "whaaaaaaaat?: " ++ symbolVal proxy

-- Note a typo in this one won't be caught -- it'll just use the default value
uaArgValWDefault :: (FromUA as, KnownSymbol aToLookUp, ToSig defaultVal (SDBodyArgs as)) => defaultVal -> as -> proxy aToLookUp -> SDBody as Signal
uaArgValWDefault defaultVal uaArgs proxy = do
   allSigs <- fromUA uaArgs
   case L.lookup (symbolVal proxy) (allSigs::[(String,Signal)]) of
      Just x -> return x
      Nothing -> toSig defaultVal

-- instance (args0 ~ args1) => FromUA (NoDefaults args0) args1 where
instance FromUA (NoDefaults args0) where
   type UAsArgs (NoDefaults args0) = '[]
   type SDBodyArgs (NoDefaults args0) = args0
   fromUA _ = return []

instance FromUA (UA a args) where
   type UAsArgs (UA a args) = '[a]
   type SDBodyArgs (UA a args) = args
   fromUA :: UA a args -> SDBody (UA a args) [(String, Signal)]
   fromUA (UA x) = do
      y <- x
      return [(symbolVal (Proxy::Proxy a), y)]
      -- The LHS is like "freq" and the RHS is the value -- lhs isn't the 'foo' in 'A::A "foo"' ^^

instance (args0 ~ args1, KnownSymbol a {-, KnownSymbol b -}) => FromUA (UA a args0, UA b args1) where
   type UAsArgs (UA a args0, UA b args1) = '[a, b]
   type SDBodyArgs (UA a args0, UA b args1) = args0
   fromUA (a, b) = (<>) <$> fromUA a <*> fromUA b

instance (AllEqual '[as0,as1,as2], KnownSymbol a, KnownSymbol b)
         => FromUA (UA a as0, UA b as1, UA c as2) where
   type UAsArgs (UA a as0, UA b as1, UA c as2) = '[a, b, c]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2) = as0
   fromUA (a, b, c) =
      -- (<>) <$> fromUA a <*> fromUA (b, c)
      concat <$> sequence [fromUA a, fromUA b, fromUA c]


instance (AllEqual '[as0,as1,as2,as3,as4], AllKnownSymbols '[a, b, c])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3) = '[a, b, c, d]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3) = as0
   fromUA (a, b, c, d) =
      -- (<>) <$> fromUA (a,b) <*> fromUA (c,d)
      concat <$> sequence [fromUA a,fromUA b,fromUA c, fromUA d]



instance (AllEqual '[as0,as1,as2,as3,as4], AllKnownSymbols '[a,b,c,d] -- , KnownSymbol e --,
--     (SDBodyArgs (UA a as0)) ~ (UghRename (UA b as1, UA c as2, UA d as3, UA e as4))
          )
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4) = '[a, b, c, d, e]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4) = as0
   fromUA (a, b, c, d, e) =
      -- (<>) <$> fromUA (a,b,c) <*> fromUA (d,e)
      concat <$> sequence [fromUA a,fromUA b,fromUA c,fromUA d,fromUA e]

-- 6:
instance (AllEqual '[as0,as1,as2,as3,as4,as5], AllKnownSymbols '[a,b,c,d,e,f])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5) = '[a, b, c, d, e, f]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5) = as0
   fromUA (a, b, c, d, e, f) =
      -- (<>) <$> fromUA (a,b,c) <*> fromUA (d,e,f)
      concat <$> sequence [fromUA a,fromUA b,fromUA c,fromUA d,fromUA e
         ,fromUA f]

-- 7:
instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6], AllKnownSymbols '[a,b,c,d,e,f,g])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5, UA g as6) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6) = '[a, b, c, d, e, f, g]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6) = as0
   fromUA (a,b,c,d,e,f,g) =
      -- concat <$> sequence [fromUA (a,b,c),fromUA (d,e,f), fromUA g]
      -- (<>) <$> fromUA (a,b,c,d) <*> fromUA (e,f,g)
      concat <$> sequence [fromUA a,fromUA b,fromUA c, fromUA d,fromUA e
         ,fromUA f, fromUA g]

-- 8
instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7]
         , AllKnownSymbols '[a,b,c,d,e,f,g,h])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6,UA h as7) = '[a, b, c, d, e, f, g, h]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6,UA h as7) = as0
   fromUA (a,b,c,d,e,f,g,h) =
      -- (<>) <$> fromUA (a,b,c,d) <*> fromUA (e,f,g,h)
      -- concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h)]
      concat <$> sequence [fromUA a,fromUA b,fromUA c, fromUA d,fromUA e
         ,fromUA f, fromUA g, fromUA h]

-- 9:
instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6,UA h as7,UA i as8) = '[a, b, c, d, e, f, g, h, i]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6,UA h as7,UA i as8) = as0
   fromUA (a,b,c,d,e,f,g,h,i) =
      -- (<>) <$> fromUA (a,b,c,d,e) <*> fromUA (f,g,h,i)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i)]


instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9) =
          '[a,b,c,d,e,f,g,h,i,j]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6,UA h as7,UA i as8,UA j as9) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j) =
      -- (<>) <$> fromUA (a,b,c,d,e) <*> fromUA (f,g,h,i,j)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j)]

instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10) =
          '[a,b,c,d,e,f,g,h,i,j,k]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k) =
      -- (<>) <$> fromUA (a,b,c,d,e,f) <*> fromUA (g,h,i,j,k)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k)]


instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11) =
          '[a,b,c,d,e,f,g,h,i,j,k,l]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l) =
      -- (<>) <$> fromUA (a,b,c,d,e,f) <*> fromUA (g,h,i,j,k,l)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l)]


instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11,UA m as12) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g) <*> fromUA (h,i,j,k,l,m)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m)]


instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11,UA m as12,UA n as13) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g) <*> fromUA (h,i,j,k,l,m,n)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n)]


instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14
                ) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14
                  ) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h) <*> fromUA (i,j,k,l,m,n,o)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o)]


instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15
                ) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15
                  ) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h) <*> fromUA (i,j,k,l,m,n,o,p)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p)]


instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                ) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                  ) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i) <*> fromUA (j,k,l,m,n,o,p,q)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q)]


instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16,as17]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ,UA r as17
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                ) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                  ) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i) <*> fromUA (j,k,l,m,n,o,p,q,r)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q,r)]


instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16,as17,as18]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ,UA r as17, UA s as18
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                ,UA s as18) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                  ,UA s as18) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i,j) <*> fromUA (k,l,m,n,o,p,q,r,s)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q,r), fromUA (s)]

instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16,as17,as18,as19]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ,UA r as17, UA s as18,UA t as19
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                ,UA s as18,UA t as19) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                  ,UA s as18,UA t as19) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i,j) <*> fromUA (k,l,m,n,o,p,q,r,s,t)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q,r), fromUA (s,t)]

instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16,as17,as18,as19,as20]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ,UA r as17, UA s as18,UA t as19,UA u as20
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                ,UA s as18,UA t as19,UA u as20
                ) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                  ,UA s as18,UA t as19,UA u as20
                  ) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i,j,k) <*> fromUA (l,m,n,o,p,q,r,s,t,u)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q,r), fromUA (s,t,u)]

instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16,as17,as18,as19,as20,as21]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ,UA r as17, UA s as18,UA t as19,UA u as20,UA v as21
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                ,UA s as18,UA t as19,UA u as20,UA v as21
                ) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                  ,UA s as18,UA t as19,UA u as20,UA v as21
                  ) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i,j,k) <*> fromUA (l,m,n,o,p,q,r,s,t,u,v)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q,r), fromUA (s,t,u), fromUA (v)]

instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16,as17,as18,as19,as20,as21,as22]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ,UA r as17, UA s as18,UA t as19,UA u as20,UA v as21
                   ,UA w as22
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                ,UA s as18,UA t as19,UA u as20,UA v as21,UA w as22
                ) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                  ,UA s as18,UA t as19,UA u as20,UA v as21,UA w as22
                  ) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i,j,k,l) <*> fromUA (m,n,o,p,q,r,s,t,u,v,w)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q,r), fromUA (s,t,u), fromUA (v,w)]

instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16,as17,as18,as19,as20,as21,as22,as23]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ,UA r as17, UA s as18,UA t as19,UA u as20,UA v as21
                   ,UA w as22,UA x as23
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                ,UA s as18,UA t as19,UA u as20,UA v as21,UA w as22,UA x as23
                ) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                  ,UA s as18,UA t as19,UA u as20,UA v as21,UA w as22,UA x as23
                  ) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i,j,k,l) <*> fromUA (m,n,o,p,q,r,s,t,u,v,w,x)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q,r), fromUA (s,t,u), fromUA (v,w,x)]

instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16,as17,as18,as19,as20,as21,as22,as23,as24]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ,UA r as17, UA s as18,UA t as19,UA u as20,UA v as21
                   ,UA w as22,UA x as23,UA y as24
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                ,UA s as18,UA t as19,UA u as20,UA v as21,UA w as22,UA x as23
                ,UA y as24) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                  ,UA s as18,UA t as19,UA u as20,UA v as21,UA w as22,UA x as23
                  ,UA y as24) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m) <*> fromUA (n,o,p,q,r,s,t,u,v,w,x,y)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q,r), fromUA (s,t,u), fromUA (v,w,x), fromUA (y)]

instance (AllEqual '[as0,as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11,as12,as13,as14,as15,as16,as17,as18,as19,as20,as21,as22,as23,as24,as25]
         ,AllKnownSymbols '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z])
         => FromUA (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                   ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                   ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16
                   ,UA r as17, UA s as18,UA t as19,UA u as20,UA v as21
                   ,UA w as22,UA x as23,UA y as24,UA z as25
                   ) where
   type UAsArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                ,UA s as18,UA t as19,UA u as20,UA v as21,UA w as22,UA x as23
                ,UA y as24,UA z as25) =
          '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
   type SDBodyArgs (UA a as0, UA b as1, UA c as2, UA d as3, UA e as4, UA f as5
                  ,UA g as6,UA h as7,UA i as8,UA j as9,UA k as10,UA l as11
                  ,UA m as12,UA n as13,UA o as14,UA p as15,UA q as16,UA r as17
                  ,UA s as18,UA t as19,UA u as20,UA v as21,UA w as22,UA x as23
                  ,UA y as24,UA z as25) = as0
   fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) =
      -- (<>) <$> fromUA (a,b,c,d,e,f,g,h,i,j,k,l,m) <*> fromUA (n,o,p,q,r,s,t,u,v,w,x,y,z)
      concat <$> sequence [fromUA (a,b,c), fromUA (d,e,f), fromUA (g,h,i), fromUA (j,k,l), fromUA (m,n,o), fromUA (p,q,r), fromUA (s,t,u), fromUA (v,w,x), fromUA (y,z)]


-- | \"UGen Arg\"
data UA (name :: Symbol) (args :: [Symbol]) =
   KnownSymbol name => UA (SDBody' args Signal)


type family AllEqual (a :: [[Symbol]]) :: Constraint where
   AllEqual '[] = ()
   AllEqual (a ': '[b]) = (a ~ b)
   AllEqual (a ': b ': cs) = (a ~ b, (AllEqual (a ': cs)))

type family AllKnownSymbols (a :: [ks]) :: Constraint where
   AllKnownSymbols '[] = ()
   AllKnownSymbols (ks0 ': kss) = (KnownSymbol ks0, AllKnownSymbols kss)

type family EachElems (forEach :: [Symbol]) args :: Constraint where
   EachElems (x ': xs) args = (Elem x args, EachElems xs args)
   EachElems '[] args = ()

type family Args (required :: [Symbol])
                 (optional :: [Symbol])
                 args
            :: Constraint where
   Args required optional args =
      ( Subset required (UAsArgs args)
      , Subset (UAsArgs args) (SetUnion required optional)
      , FromUA args
      -- , EachElems required (UAsArgs args)
      -- , EachElems optional (UAsArgs args)
      )

