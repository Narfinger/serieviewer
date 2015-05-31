{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module XMLHandler ( readSeries
                  , writeSeries)
       where

import Data.Map
import Data.UUID
import Text.XML.HXT.Core
import Serie (Serie(..))



instance XmlPickler Serie where
  xpickle = xpSerie

instance XmlPickler UUID where
 xpickle = xpPrim

uuidFromMaybe :: Maybe UUID -> UUID
uuidFromMaybe Nothing   = nil
uuidFromMaybe (Just u)  = u

xpUUID :: PU UUID
xpUUID = xpWrap ( uuidFromMaybe . fromString
                , toString) xpText

xpSerie :: PU Serie
xpSerie = xpElem "serie" $
          xpWrap ( \ (( d,e,m,o,t,u)) -> Serie d e m o t u
                 , \ t -> (dir t, episode t, maxepisode t
                          , ongoing t, title t, uuid t
                          )
                 ) $
          xp6Tuple (xpAttr "dir" xpText)
                   (xpAttr "episode" xpPrim)
                   (xpAttr "max" xpPrim)
                   (xpAttr "ongoing" xpPrim)
                   (xpAttr "title" xpText)
                   (xpAttr "uuid" xpUUID)

xpSeries :: PU [Serie]
xpSeries = xpElem "series" $
           xpList xpickle

processSerie :: IOSArrow [Serie] [Serie]
processSerie
    = arrIO ( \ x -> do {print x ; return x})


readSeries :: FilePath -> IO [Serie]
readSeries fp = do
  s<- runX ( xunpickleDocument xpSeries
                               [ withValidate no
                               , withTrace 1
                               , withRemoveWS yes
                               , withPreserveComment no
                               ] fp
           );
    return $ head s ;

writeSeries :: FilePath -> [Serie] -> IO ()
writeSeries fp serie = do
  runX (constA serie >>> xpickleDocument xpSeries [ withIndent yes] fp);
  return ()
