{-# LANGUAGE TemplateHaskell #-}
module Eff.Fresh (
      Fresh(..)
    , freshVar
    , freshVar2
    , freshVar3
    
    , runFresh
    , runFreshM
    , runFreshName
    ) where

import Eff.Prelude

data Fresh u q m a where
    FreshVar :: u -> Fresh u q m q

makeEffect ''Fresh

freshVar2 :: (Fresh a1 b1 :> es, Fresh a2 b2 :> es) => a1 -> a2 -> Eff es (b1, b2)
freshVar2 x y = (,) <$> freshVar x <*> freshVar y

freshVar3 :: Members [Fresh a1 b1, Fresh a2 b2, Fresh a3 b3] r => a1 -> a2 -> a3 -> Eff r (b1, b2, b3)
freshVar3 x y z = (,,) <$> freshVar x <*> freshVar y <*> freshVar z

runFreshName :: Eff (Fresh Text Name : r) a -> Eff r a
runFreshName = evalState (0 :: Int) . reinterpret \case
    FreshVar x -> do
        i <- get @Int
        put (i + 1)
        pure (x <> "_" <> show i)

runFresh :: (u -> q) -> Eff (Fresh u q : r) a -> Eff r a
runFresh f = runFreshM (pure . f)

runFreshM :: (u -> Eff r q) -> Eff (Fresh u q : r) a -> Eff r a
runFreshM f = interpret \case
    FreshVar x -> f x
