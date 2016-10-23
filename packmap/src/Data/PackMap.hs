{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveTraversable #-}
module Data.PackMap where
import qualified Data.IntMap.Strict as M
import Control.Lens
import Data.Semigroup
import Data.Profunctor.Unsafe
import Unsafe.Coerce
import Data.Witherable

class Packable a where
    _Pack :: Prism' Int a

instance Packable Int where
    _Pack = id

newtype PackMap k a = PackMap { toIntMap :: M.IntMap a } deriving (Eq, Ord, Functor, Foldable, Traversable)

type instance Index (PackMap k a) = k

type instance IxValue (PackMap k a) = a

instance AsEmpty (PackMap k a) where
    _Empty = dimap toIntMap (fmap PackMap) . _Empty

instance Packable k => Ixed (PackMap k a) where
    ix i f = unsafeFmapPackMap . ix (_Pack # i) f .# toIntMap
    {-# INLINE ix #-}

instance Packable k => At (PackMap k a) where
    at i f = unsafeFmapPackMap . at (_Pack # i) f .# toIntMap

instance Semigroup a => Monoid (PackMap k a) where
    mempty = PackMap M.empty
    mappend (PackMap a) (PackMap b) = PackMap $ M.unionWith (<>) a b

unsafeFmapPackMap :: Functor f => f (M.IntMap a) -> f (PackMap k a)
unsafeFmapPackMap = unsafeCoerce

instance Packable k => FunctorWithIndex k (PackMap k) where
    imap f = unsafeFmapPackMap $ imap (f . unsafeView _Pack) .# toIntMap

instance Packable k => FoldableWithIndex k (PackMap k) where
    ifoldMap f = ifoldMap (f . unsafeView _Pack) .# toIntMap

instance Packable k => TraversableWithIndex k (PackMap k) where
    itraverse f = unsafeFmapPackMap . itraverse (f . unsafeView _Pack) .# toIntMap

newtype UnsafeUnique a b = UnsafeUnique { safeUnique :: a } deriving Functor

instance Applicative (UnsafeUnique b) where
    pure = error "pure :: a -> UnsafeUnique b a"
    (<*>) = error "(<*>) :: UnsafeUnique r (a -> b) -> UnsafeUnique r a -> UnsafeUnique r b"

unsafeView :: ((a -> UnsafeUnique a a) -> (s -> UnsafeUnique a s)) -> s -> a
unsafeView l = safeUnique . l UnsafeUnique

instance Witherable (PackMap k) where
    wither f = unsafeFmapPackMap . wither f .# toIntMap
    mapMaybe f = unsafeFmapPackMap $ M.mapMaybe f .# toIntMap
    filter f = unsafeFmapPackMap $ M.filter f .# toIntMap

fromList :: Packable k => [(k, a)] -> PackMap k a
fromList = unsafeFmapPackMap $ M.fromList . map (_1 %~ review _Pack)
