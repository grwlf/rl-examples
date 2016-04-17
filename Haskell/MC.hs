{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module MC where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Random
import Control.Break
import Control.Lens (makeLenses, (%=), view, use, uses)
import Data.Ratio
import Data.List hiding (break)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.Foldable
import Prelude hiding(break)
import System.Random
import Text.Printf

import Types as RL

class (Ord s) => MC_Problem pr s a | pr -> s , pr -> a where
  mc_terminal :: pr -> s -> Bool
  mc_actions :: pr -> s -> Set a
  mc_transition :: (RandomGen g) => pr -> s -> a -> g -> (s,g)
  mc_reward :: pr -> s -> a -> s -> Reward

class (MC_Problem pr s a) => MC_Policy pr s a p where
  mcp_action :: (RandomGen g) => pr -> s -> p -> g -> (a,g)

episode :: (RandomGen g , MC_Policy pr s a p) => pr -> s -> p -> g -> ([s],g)
episode pr s p g =
  flip runRand g $ do
  flip execStateT [] $ do
  loop $ do
    let rnd = lift . lift . liftRand
    a <- rnd $ mcp_action pr s p
    s <- rnd $ mc_transition pr s a
    modify (s:)
    when (mc_terminal pr s) $ do
      break ()







