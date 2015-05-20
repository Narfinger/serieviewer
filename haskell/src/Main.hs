module Main where

import Control.Monad (msum)
import qualified Happstack.Server as H

import Pages (indexPage, playPage)
import Serie (Serie(..))
import XMLHandler


testseries = [ Serie { dir = "/tmp", episode = 1, maxepisode = 5, ongoing = False, title = "Test this onece" }
             , Serie { dir = "/tmp", episode = 3, maxepisode = 10, ongoing = False, title = "Test this twice" }
             , Serie { dir = "/tmp", episode = 1, maxepisode = 3, ongoing = True, title = "Test this thrice" }]

giveS = return (concat [testseries,testseries])

main :: IO ()
main = do
  let conf = H.nullConf
      addr = "0.0.0.0"
  putStrLn "Starting server";
  s <- H.bindIPv4 addr (H.port conf);
  series <- return testseries;
  H.simpleHTTPWithSocket s conf $ msum
       [ H.dir "style.css" $ H.serveFile (H.asContentType "text/css") "static/style.css"
       , H.dir "static"    $ H.serveDirectory H.EnableBrowsing [] "static/"
--       , H.dir "execute"   $ H.
       , H.dir "execute"   $ H.dir "play" $ H.path $ \s -> do series <- giveS; H.ok $ H.toResponse $ playPage s
       , H.ok $ H.toResponse $ indexPage series
       ]
