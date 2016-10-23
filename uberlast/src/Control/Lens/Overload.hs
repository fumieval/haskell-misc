{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses, DataKinds, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Overload
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Control.Lens.Overload where

import Language.Haskell.TH
import Language.Haskell.TH.Lens
import Data.Proxy
import Control.Lens
import GHC.TypeLits

-- | @a@ has a field named @s@
class Has (s :: Symbol) a where
  type Field s a :: *
  theField :: Proxy s -> Lens' a (Field s a)

-- | Generate 'Has' instances from data type declaration.
-- @
-- declareHas [d|data Stuff = Stuff { position :: V3 Float, velocity :: V3 Float, weight :: Float}|]
-- @
-- will create
-- @
-- 
-- @
-- data Stuff = Stuff (V3 Float) (V3 Float) Float
-- instance Has "position" Stuff where
--   type Field "position" Stuff = V3 Float
--   theField = ...
-- instance Has "velocity" Stuff where
--   type Field "velocity" Stuff = V3 Float
--   theField = ...
-- instance Has "weight" Stuff where
--   type Field "weight" Stuff = Float
--   theField = ...
-- @
declareHas :: DecsQ -> DecsQ
declareHas decs = declareLenses decs >>= \r -> case r of
  (d@(DataD _ dName _ _ _):sfs) -> return $ d : pairs (gen dName) sfs
  where
    pairs f (x:y:_:zs) = f x y : pairs f zs
    pairs _ [] = []
    gen dName (SigD n (ForallT _ _ (AppT _ t))) (FunD _ cs) = InstanceD []
      (ConT ''Has `AppT` LitT (StrTyLit $ nameBase n) `AppT` ConT dName) [
      TySynInstD ''Field $ TySynEqn [LitT (StrTyLit $ nameBase n), ConT dName] t
      , FunD 'theField $ map (clausePattern %~ (WildP:)) cs
      , PragmaD $ InlineP 'theField Inline FunLike AllPhases
      ]

-- | Create an alias of 'theField' so that you don't have to specify a type signature by hand.
makeField :: String -> DecsQ
makeField s = do
  let tvA = mkName "a"
  return [SigD (mkName s) $ ForallT [PlainTV tvA] [ClassP ''Has [LitT (StrTyLit s), VarT tvA]]
    $ ConT ''Lens' `AppT` VarT tvA `AppT` (ConT ''Field `AppT` LitT (StrTyLit s) `AppT` VarT tvA)
    , ValD (VarP $ mkName s)
    (NormalB $ VarE 'theField `AppE` (ConE 'Proxy `SigE` (ConT ''Proxy `AppT` LitT (StrTyLit s)))) []]
