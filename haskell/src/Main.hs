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
import Data.UUID
import qualified Happstack.Server as H

import System.Directory
import System.FilePath
import System.Process
import Pages (indexPage, playPage, modifyPage)
import qualified Serie as S
import Utils (replaceElementInList)
import XMLHandler

type Series = [S.Serie]

newtype App a = App { unApp :: H.ServerPartT (StateT (TVar Series) IO) a }
    deriving ( Functor, Alternative, Applicative, Monad                
             , MonadPlus, MonadIO, H.HasRqData, H.ServerMonad
             , H.WebMonad H.Response, H.FilterMonad H.Response
             , H.Happstack, MonadState (TVar [S.Serie]) )

testseries = [ S.Serie { S.dir = "/tmp/test", S.episode = 1, S.maxepisode = 5,  S.ongoing = False, S.title = "Test this onece", S.uuid = nil}
             , S.Serie { S.dir = "/tmp", S.episode = 3, S.maxepisode = 10, S.ongoing = False, S.title = "Test this twice",  S.uuid = nil}
             , S.Serie { S.dir = "/tmp", S.episode = 1, S.maxepisode = 3,  S.ongoing = True,  S.title = "Test this thrice",  S.uuid = nil}]


runUpdatePage :: (MonadIO m, MonadState (TVar [a]) m, H.FilterMonad H.Response m) => Int -> (a -> Maybe a) -> (a -> IO a1) -> m H.Response
runUpdatePage number updatef runf = do
  tvar <- get;
  series <- liftIO $ readTVarIO tvar;
  let s = series !! number
  let nx = updatef s
  let nxs = replaceElementInList series number nx
  liftIO $ runf s;
  liftIO $ atomically $ writeTVar tvar nxs;
  H.seeOther ("/"::String) (H.toResponse ("" ::String))

emptyFun :: t -> IO ()
emptyFun s =  return ()

index :: App H.Response
index = do
  tvar <- get;
  series <- liftIO $ readTVarIO tvar;
  H.ok $ H.toResponse $ indexPage series

modify :: Int -> App H.Response
modify n = do
  tvar <- get;
  series <- liftIO $ readTVarIO tvar;
  let s = series !! n
  H.ok $ H.toResponse $ modifyPage s

runApp :: TVar Series -> App a -> H.ServerPartT IO a
runApp series (App sp) = H.mapServerPartT (`evalStateT` series) sp -- (flip evalStateT series) sp

playSerie :: Int -> App H.Response
playSerie number = runUpdatePage number S.incrementEpisode S.playCurrentEpisode 

changeSerie :: Int -> App H.Response
changeSerie number = runUpdatePage number S.incrementEpisode emptyFun

routing :: TVar Series -> H.ServerPartT IO H.Response
routing series = msum
       [ H.dir "style.css" $ H.serveFile (H.asContentType "text/css") "static/style.css"
       , H.dir "static"    $ H.serveDirectory H.EnableBrowsing [] "static/"
       , H.dir "execute"   $ H.dir "play"   $ H.path $ \n -> runApp series (playSerie n)
       , H.dir "execute"   $ H.dir "change" $ H.path $ \n -> runApp series (changeSerie n)
--       , H.dir "modify"    $ runApp series modify 
       , H.dir "modify" $ H.path $ \n -> runApp series (modify n)
       , runApp series index
       ]

main :: IO ()
main = do
  let conf = H.nullConf
      addr = "127.0.0.1"
  putStrLn "Starting server";
  s <- H.bindIPv4 addr (H.port conf);
  series <- atomically $ newTVar testseries;
  H.simpleHTTPWithSocket s conf (routing series) 
