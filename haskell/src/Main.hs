{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, 
     TemplateHaskell, TypeFamilies, DeriveDataTypeable, 
     FlexibleContexts, ScopedTypeVariables, 
     NamedFieldPuns, DeriveFunctor, StandaloneDeriving, OverloadedStrings,
     RecordWildCards #-}

module Main where

import Control.Applicative         ( Applicative, Alternative, (<$>))
import Control.Monad               ( msum, MonadPlus )
import Control.Monad.State.Strict  ( MonadState, StateT, get, put,  evalStateT )
import Control.Monad.Trans         ( MonadIO, liftIO )
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Happstack.Server as H

import Pages (indexPage, playPage)
import qualified Serie as S
import XMLHandler

type Series = [S.Serie]

newtype App a = App { unApp :: H.ServerPartT (StateT (TVar Series) IO) a }
    deriving ( Functor, Alternative, Applicative, Monad                
             , MonadPlus, MonadIO, H.HasRqData, H.ServerMonad
             , H.WebMonad H.Response, H.FilterMonad H.Response
             , H.Happstack, MonadState (TVar [S.Serie]) )

testseries = [ S.Serie { S.dir = "/tmp", S.episode = 1, S.maxepisode = 5,  S.ongoing = False, S.title = "Test this onece" }
             , S.Serie { S.dir = "/tmp", S.episode = 3, S.maxepisode = 10, S.ongoing = False, S.title = "Test this twice" }
             , S.Serie { S.dir = "/tmp", S.episode = 1, S.maxepisode = 3,  S.ongoing = True,  S.title = "Test this thrice" }]


updateList :: [a] -> Int -> a -> [a]
updateList list number element =
  let (h,t) = splitAt number list in
    let dropped = drop 0 t in
     h ++ (element : dropped)

--runUpdatePage :: Int -> App H.Response
runUpdatePage number f = do
  tvar <- get;
  series <- liftIO $ readTVarIO tvar;
  let s = series !! number
  let nx = f s
  let nxs = updateList series number nx
  H.ok $ H.toResponse $ indexPage testseries

playSerie :: Int -> App H.Response
playSerie number = do
  tvar <- get;
  series <- liftIO $ readTVarIO tvar;
  let s = series !! number
  liftIO $ atomically $ writeTVar tvar (testseries ++ testseries);
  --  S.playCurrentEpisode s;
  H.ok $ H.toResponse $ indexPage testseries

runApp :: TVar Series -> App a -> H.ServerPartT IO a
runApp series (App sp) = do
  H.mapServerPartT (flip evalStateT series) sp

      -- figure out type
routing series = msum
       [ H.dir "style.css" $ H.serveFile (H.asContentType "text/css") "static/style.css"
       , H.dir "static"    $ H.serveDirectory H.EnableBrowsing [] "static/"
       , H.dir "execute"   $ H.dir "play" $ runApp series (playSerie 1)
--       , H.dir "execute"   $ H.
       , H.ok $ H.toResponse $ indexPage testseries
       ]

main :: IO ()
main = do
  let conf = H.nullConf
      addr = "127.0.0.1"
  putStrLn "Starting server";
  s <- H.bindIPv4 addr (H.port conf);
  series <- atomically $ newTVar testseries;
  H.simpleHTTPWithSocket s conf (routing series) 
