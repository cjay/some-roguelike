{-# LANGUAGE FlexibleContexts           #-}

module ApecsExts where

import           Apecs
import           Apecs.Core
import           Control.Monad
import           Safe                         (headMay)

-- | Extract list of values from a component pattern, one value per matching entity.
extractAll :: forall w m c a. (Members w m c, Get w m c) => (c -> a) -> SystemT w m [a]
extractAll selector = cfold (\accum component -> selector component : accum) []

-- | Extract a value from a component pattern if a matching entity exists.
extract :: forall w m c a. (Members w m c, Get w m c) => (c -> a) -> SystemT w m (Maybe a)
extract = fmap headMay . extractAll

{-# INLINE cmapOver #-}
cmapOver :: forall w m cx cy. (Get w m cx, Set w m cy)
         => [Entity] -> (cx -> cy) -> SystemT w m ()
cmapOver es f = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  lift $
    forM_ es $ \(Entity e) ->
      explExists sx e >>= flip when (do
        r <- explGet sx e
        explSet sy e (f r))

{-# INLINE cmapOverM #-}
cmapOverM :: forall w m cx cy. (Get w m cx, Set w m cy)
          => [Entity] -> (cx -> SystemT w m cy) -> SystemT w m ()
cmapOverM es sys = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  forM_ es $ \(Entity e) ->
    lift (explExists sx e) >>= flip when (do
      x <- lift $ explGet sx e
      y <- sys x
      lift $ explSet sy e y)

{-# INLINE cfoldOver #-}
cfoldOver :: forall w m c a. (Get w m c)
          => [Entity] -> (a -> c -> a) -> a -> SystemT w m a
cfoldOver es f a0 = do
  s :: Storage c <- getStore
  lift $ foldM (\a (Entity e) -> f a <$> explGet s e) a0 es
