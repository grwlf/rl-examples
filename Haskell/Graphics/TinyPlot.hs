{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.TinyPlot where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char
import System.IO
import System.Process
import System.FilePath
import Text.Heredoc
import Text.Printf

data PlotData = PlotData {
    ps_filename :: String
  , ps_handle :: Handle
  } deriving(Show)

newData :: FilePath -> IO PlotData
newData filename = PlotData filename <$> openFile filename WriteMode

push :: PlotData -> Double -> Double -> IO ()
push PlotData{..} x y = do
  hPutStrLn ps_handle (show x ++ "\t" ++ show y) >> hFlush ps_handle

dat :: PlotData -> String
dat PlotData{..} = printf "\"%s\"" ps_filename


data Plot = Plot {
  pl_handle :: ProcessHandle
}

spawnPlot :: String -> String -> IO Plot
spawnPlot name plot = do
  Plot <$> do
    writeFile (name -<.> ".gnuplot") plot *> spawnProcess "gnuplot" [name]


test = do
  d <- newData "plot.dat"

  spawnPlot "plot1" [heredoc|
    set xrange [0:20]
    set yrange [0:400]
    plot ${dat d} using 1:2 with lines
    pause 1
    reread
  |]

  forM_ [0..100] $ \i@(fromInteger -> r) -> do
    when (i`mod`10 == 0) $ do
      threadDelay (10^6)
    push d r (r*r  / 3.2)

