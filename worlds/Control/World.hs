{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.World (Name, spawn, kill, (.!), (.?), WorldT, runWorldT, Life(..), Karma) where

import qualified Data.Traversable as T
import Control.Monad.Trans.Operational.Mini
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.IntMap.Strict as IM
import Control.Monad.Identity
import Control.Lens
import Control.Monad.State
import Control.Applicative
import Data.Void

newtype Name s (e :: * -> *) a = Name Int

spawn :: Monad m => (Karma s (WorldT s n) :! m) => Life (WorldT s n) e r -> m (Name s e r)
spawn l = singleton (Spawn l)

kill :: Monad m => (Karma s (WorldT s n) :! m) => Name s e r -> r -> m ()
kill n r = singleton (Kill n r)

(.?) :: (Karma s (WorldT s n) :! m) => Name s e r -> e a -> m (Maybe a)
n .? e = singleton (Contact n e)

(.!) :: (Karma s (WorldT s n) :! m) => Name s e Void -> e a -> m a
n .! e = (n .? e) >>= maybe (fail "Impossible") return

-- | The type of lives.
-- @m@ is a world where the life belongs.
-- @e@ is a type of effects that the life can handle.
-- @r@ is an end of life. Put '()' for mortals, 'Void' for immortals.

data Life m e r = Dead r | Alive (forall a. e a -> m (a, Life m e r))

data Karma s m x where
    Spawn :: Life m e r -> Karma s m (Name s e r)
    Contact :: Name s e r -> e a -> Karma s m (Maybe a)
    Kill :: Name s e r -> r -> Karma s m ()

newtype WorldT s m a = WorldT { unWorld :: ReifiedProgramT (Karma s (WorldT s m)) m a } deriving (Functor, Applicative, Monad)

instance MonadTrans (WorldT s) where
    lift = WorldT . lift

instance MonadState s m => MonadState s (WorldT s' m) where
    get = lift get
    put = lift . put

instance Monad m => Karma s (WorldT s m) :! WorldT s m where
    singleton = WorldT . singleton 

execWorld :: Monad m => WorldT s m a -> StateT (Int, IM.IntMap (Life (WorldT s m) Any Any)) m a
execWorld = go . unWorld where
    go (Return a) = return a
    go (Spawn l :>>= cont) = do
        n <- use _1
        _2 %= IM.insert n (unsafeCoerce l)
        _1 += 1
        go $ cont $ Name n
    go (Kill (Name i) r :>>= cont) = do
        _2 %= IM.delete i
        go $ cont ()
    go (Contact (Name i) e :>>= cont) = do
        m <- use _2
        case IM.lookup i m of
            Nothing -> go $ cont Nothing
            Just (Dead _) -> go $ cont Nothing
            Just (Alive f) -> do
                (a, l') <- execWorld (unsafeCoerce f e)
                _2 %= IM.insert i l'
                go $ cont (Just a)
    go (Lift m cont) = lift m >>= go . cont

-- | Run everything
runWorldT :: Monad m => WorldT s m a -> m a
runWorldT m = evalStateT (execWorld m) (0, IM.empty)