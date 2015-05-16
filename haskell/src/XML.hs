{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module XMLSerie where
import Serie (Serie(..))

import Data.Map

import System.Directory
import System.FilePath

import Text.XML.HXT.Core

instance XmlPickler Serie where
  xpickle = xpSerie

type Series = [Serie]

xpSerie :: PU Serie
xpSerie = xpElem "serie" $
          xpWrap ( \ (( d,e,m,o,t)) -> Serie d e m o t
                 , \ t -> (dir t, episode t, maxepisode t
                          , ongoing t, title t
                          )
                 ) $
          xp5Tuple (xpAttr "dir" xpText)
                   (xpAttr "episode" xpPrim)
                   (xpAttr "max" xpPrim)
                   (xpAttr "ongoing" xpPrim)
                   (xpAttr "title" xpText)

xpSeries :: PU Series
xpSeries = xpElem "series" $
           xpList xpickle

processSerie :: IOSArrow Series Series
processSerie
    = arrIO ( \ x -> do {print x ; return x})


readSerie :: FilePath -> IO Series
readSerie fp = do
  s<- runX ( xunpickleDocument xpSeries
                               [ withValidate no
                               , withTrace 1
                               , withRemoveWS yes
                               , withPreserveComment no
                               ] fp
           );
    return $ s !! 0;

writeSerie :: FilePath -> Series -> IO ()
writeSerie fp serie = do
  runX (constA serie >>> xpickleDocument xpSeries [ withIndent yes] fp);
  return ()
