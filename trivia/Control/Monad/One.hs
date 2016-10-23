{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Control.Monad.One (One(..)) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Distributive
import Control.Monad
import Data.Typeable

data One a = One deriving (Show, Read, Eq, Ord, Enum, Functor, Foldable, Traversable, Typeable)

instance Monoid (One a) where
    mempty = One
    mappend _ _ = One

instance Monad One where
    return _ = One
    {-# INLINE return #-}
    _ >>= _ = One
    {-# INLINE (>>=) #-}

instance Applicative One where
    pure _ = One
    {-# INLINE pure #-}
    _ <*> _ = One
    {-# INLINE (<*>) #-}

instance Alternative One where
    empty = One
    {-# INLINE empty #-}
    _ <|> _ = One
    {-# INLINE (<|>) #-}

instance MonadPlus One where
    mzero = One
    mplus _ _ = One

instance Distributive One where
    distribute _ = One
    {-# INLINE distribute #-}
    collect _ _ = One
    {-# INLINE collect #-}