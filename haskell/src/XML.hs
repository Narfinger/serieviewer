{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module XMLSerie where
import Serie (Serie(..))
import Text.XML.HXT.Core

instance XmlPickler Serie where
  xpickle = xpSerie



xpSerie :: PU Serie
xpSerie = xpElem "serie" $
          xpWrap ( uncurry Serie
                 , \ s -> (dir Serie)) $ 
          xpPair (xpAttr "dir" xpickle)


-- getSerie = deep (isElem >>> hasName "serie") >>> 
--   proc x -> do
--     d <- getAttrValue "dir" -< x
--     e <- getAttrValue "episode" -< x
--     m <- getAttrValue "max" -< x
--     o <- getAttrValue "ongoing" -< x
--     t <- getText -< x
--     returnA -< Serie { dir = d, episode = e, maxepisode = m, ongoing = o, title = t}

  
runTest = 
  do
    runX ( xunpickleDocument xpSeason
           [ withValidate no
           , withTrace 1
           , withRemoveWS yes
           , withPreserveComment no
           ] "~/.serieviewer.xml"




    
    -- runX (readDocument [ withValidate no] "~/.serieviewer.xml" 
    --       >>> getSerie 
    --      )

