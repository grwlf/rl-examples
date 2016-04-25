{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
module Types where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Ratio
import Data.List hiding (break)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random

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




data Histogram a = Histogram {
    hist_map :: Map a Integer
  } deriving(Show)

showHist :: (Ord a) => Histogram a -> IO ()
showHist Histogram{..} = do
  let max = fromInteger $ maximum $ map snd $ Map.toAscList hist_map
  forM_ (Map.toAscList hist_map`zip`[0..]) $ \((k,fromInteger -> v),i) -> do
    putStrLn $ show i ++ " " ++ (replicate ((v * 50)`div`max) '#')

sample :: (Ord a) => Int -> (StdGen -> (a,StdGen)) -> Histogram a
sample n f =
  (Histogram . fst) $
  flip runRand (mkStdGen 33) $ do
  flip execStateT Map.empty $ do
    replicateM_ n $ do
      x <- lift $ liftRand f
      modify $ Map.insertWith (+) x 1

