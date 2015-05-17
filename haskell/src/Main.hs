module Main where

import Control.Monad (msum)
import qualified Happstack.Server as H

import Pages (indexPage)
import Serie
import XMLHandler

main :: IO ()
main = do
  let conf = H.nullConf
      addr = "0.0.0.0"
  putStrLn "Starting server";
  s <- H.bindIPv4 addr (H.port conf); 
  H.simpleHTTPWithSocket s conf $ msum
       [ H.dir "style.css" $ H.serveFile (H.asContentType "text/css") "static/style.css"
       , H.dir "static/" $ H.serveDirectory H.EnableBrowsing [] "static/"
       , H.ok $ H.toResponse indexPage
       ]
