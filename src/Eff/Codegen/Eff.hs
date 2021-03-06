{-#LANGUAGE TemplateHaskell#-}
module Eff.Codegen.Eff where

import Eff.Prelude

data CacheState k v m a where
    Cached :: k -> CacheState k v m v
makeEffect ''CacheState


-- TODO: Identity should really not be necessary here
runCacheStateCounterOrdMap :: forall k v a r. (Ord k, Enum v) => Eff (CacheState k v : r) a -> Eff r (Map k v, a)
runCacheStateCounterOrdMap = fmap swap . runState (mempty @(Map k v)) . evalState (Identity (toEnum 0 :: v)) . reinterpret2 \case
    Cached k -> gets @(Map k v) (lookup k) >>= \case
        Just v -> pure v
        Nothing -> do
            nextV <- runIdentity <$> get @(Identity v)
            modify @(Map k v) (insert k (succ nextV))
            pure nextV

