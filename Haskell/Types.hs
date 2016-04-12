{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
module Types where

import Control.Monad
import Control.Monad.Trans
import Data.Ratio
import Data.List hiding (break)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

debug :: (MonadIO m) => String -> m ()
debug = liftIO . putStrLn

type Probability = Rational
type Reward = Rational

data StateVal s = StateVal {
    v_map :: Map s Rational
  } deriving(Show)

data GenericPolicy s a = GenericPolicy {
    gp_actions :: Map s (Set (Probability,a))
  } deriving(Eq,Ord, Show)

