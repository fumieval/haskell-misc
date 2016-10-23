{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.EnumVec (EnumVec, toVector, unsafeFromVector, (!)) where

import Prelude hiding (length)
import qualified Data.Vector as V
import Data.Functor.Rep
import Data.Distributive
import Data.Hashable
import Control.Applicative
import Data.Coerce
import Data.Proxy

newtype EnumVec t a = EV { toVector :: V.Vector a } deriving (Eq, Ord, Functor, Foldable)

instance Traversable (EnumVec t) where
  traverse f (EV v) = EV <$> traverse f v

instance Hashable a => Hashable (EnumVec t a) where
  hashWithSalt s (EV v) = V.foldl' hashWithSalt s v

unsafeFromVector :: V.Vector a -> EnumVec t a
unsafeFromVector = EV
{-# INLINE unsafeFromVector #-}

sizeEnumBounded :: forall proxy t. (Enum t, Bounded t) => proxy t -> Int
sizeEnumBounded _ = fromEnum (maxBound :: t) - fromEnum (minBound :: t) + 1
{-# INLINABLE sizeEnumBounded #-}

instance (Enum t, Bounded t) => Distributive (EnumVec t) where
  distribute = distributeRep

instance (Enum t, Bounded t) => Representable (EnumVec t) where
  type Rep (EnumVec t) = t
  tabulate f = EV $ V.fromListN (sizeEnumBounded (Proxy :: Proxy t)) $ map f [minBound..maxBound]
  index (EV v) i = V.unsafeIndex v (fromEnum i)

instance (Enum t, Bounded t) => Applicative (EnumVec t) where
  pure a = EV $ V.replicate (sizeEnumBounded (Proxy :: Proxy t)) a
  EV v <*> EV x = EV (V.zipWith id v x)

instance (Enum t, Bounded t) => Monad (EnumVec t) where
  return = pure
  (>>=) = bindRep

(!) :: Enum t => EnumVec t a -> t -> a
EV v ! t = V.unsafeIndex v (fromEnum t)
{-# INLINE (!) #-}
