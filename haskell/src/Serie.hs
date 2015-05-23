module Serie ( Serie(..)
             , playCurrentEpisode
             , incrementEpisode
             ) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Process
 
data Serie = Serie { dir :: FilePath
                   , episode :: Int
                   , maxepisode :: Int
                   , ongoing :: Bool
                   , title :: String
                   } deriving (Show, Eq)

extensions :: [String]
extensions = [".txt", ".mkv", ".mp3", ".avi"]

filterSupportedExtensions :: [FilePath] -> [FilePath]
filterSupportedExtensions =
  filter (\f -> takeExtension f `elem` extensions)

episodeList :: Serie -> IO [FilePath]
episodeList serie =
  filterSupportedExtensions <$> (getDirectoryContents $ dir serie)
  
incrementEpisode :: Serie -> Maybe Serie
incrementEpisode s =
  let max = maxepisode s
      nep = episode s + 1
      ns = s { episode = nep } in
  if nep > max then Nothing
  else Just ns
  

loadSerieFromDir :: FilePath -> [FilePath] -> Serie
loadSerieFromDir d fps =
  let filtered = filterSupportedExtensions fps
      t = (reverse $ splitPath d) !! 1 in
  Serie { dir = d, episode = 1, maxepisode = length filtered, ongoing = False, title = t} 

loadSerie :: FilePath -> IO Serie
loadSerie dir = 
  loadSerieFromDir dir <$> getDirectoryContents dir

playCurrentEpisode :: Serie -> IO ()
playCurrentEpisode s = do
  d <- episodeList s;
  let fname = d !! (episode s)
  let p = (proc "/usr/bin/kate" [fname])
               { cwd = Just $ dir s }
  putStrLn $ show fname;
  r <- createProcess p;
  return ()
  
