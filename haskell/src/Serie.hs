module Serie ( Serie(..)
             , playCurrentEpisode
             , incrementEpisode
             , serie
             ) where

import Control.Monad
import Data.List
import Data.UUID
import System.Directory
import System.FilePath
import System.Process
 
data Serie = Serie { dir :: FilePath    -- ^ main directory of the serie
                   , episode :: Int     -- ^ Current Episode (yet to play)
                   , maxepisode :: Int  -- ^ Maximum Episode
                   , ongoing :: Bool    -- ^ Is this an ongoing series?
                   , title :: String    -- ^ Title of the series
                   , uuid :: UUID       -- ^ UUID to quickly uniquely identify this series, is useful for next series
                   } deriving (Show, Eq)

-- |Constructs a serie with default parameter for maxepisode, ongoing and uuid 
serie :: String -> Int -> String -> Serie
serie d maxep t = Serie { dir = d, episode = 1, maxepisode = maxep, ongoing = False, title = t, uuid = nil }

-- |Extensions we scan for
extensions :: [String]
extensions = [".txt", ".mkv", ".mp3", ".avi"]

-- |Filters [Filepath] for elements in valid extensions
filterSupportedExtensions :: [FilePath] -> [FilePath]
filterSupportedExtensions =
  filter (\f -> takeExtension f `elem` extensions)

-- |Gives Files for a Serie in the dir which have valid extensions
episodeList :: Serie -> IO [FilePath]
episodeList serie =
  filterSupportedExtensions <$> getDirectoryContents (dir serie)

-- |Returns Serie with incremented episode or Nothing if maxepisode reached
incrementEpisode :: Serie -> Maybe Serie
incrementEpisode s =
  let max = maxepisode s
      nep = episode s + 1
      ns = s { episode = nep } in
  if nep > max then Nothing
  else Just ns
  

-- |Given Filepath and list of files, produce a Serie
loadSerieFromDir :: FilePath -> [FilePath] -> Serie
loadSerieFromDir d fps =
  let filtered = filterSupportedExtensions fps
      t = reverse (splitPath d !! 1) in
  serie d (length filtered) t

-- |Given FilePath produce a serie
loadSerie :: FilePath -> IO Serie
loadSerie dir = 
  loadSerieFromDir dir <$> getDirectoryContents dir

-- |Play current Episode in Serie
--playCurrentEpisode :: Serie -> IO ()
playCurrentEpisode s = do
  d <- episodeList s;
  let fname = d !! episode s
  let p = (proc "/usr/bin/kate" [fname])
               { cwd = Just $ dir s }
  print fname;
  r <- createProcess p;
  
