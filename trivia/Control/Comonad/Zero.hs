{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
module Control.Comonad.Zero (Zero) where
import Control.Comonad
import Data.Traversable
import Data.Foldable
import Data.Typeable

data Zero a

deriving instance Show (Zero a)
deriving instance Eq (Zero a)
deriving instance Ord (Zero a) 
deriving instance Functor Zero 
deriving instance Foldable Zero
deriving instance Traversable Zero 
deriving instance Typeable1 Zero

instance Comonad Zero where
    extract z = extract z
    extend f e = extend f e