module Util where

import Data.Time.Clock
import Data.Time.Calendar
import System.FilePath

import Imports

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay


save :: (MonadIO m) => String -> FilePath -> String -> m ()
save nm dir dat = liftIO $ do
  (y,m,d) <- date
  writeFile (dir </> (printf "%d-%d-%d-%s.txt" y m d nm)) dat
