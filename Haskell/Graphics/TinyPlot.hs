module TinyPlot where

data PlotStream = PlotStream {
    ps_filename :: String
  , ps_handle :: Handle
  } deriving(Show)

push :: PlotStream -> Double -> Double -> IO ()
push PlotStream{..} x y = hPutStrLn ps_handle $ show x ++ " " ++ show y

data Plot {
    pl_program :: String
  } deriving(Show)


spawnPlot :: Plot -> IO ()
spawnPlot = System.runProcess


