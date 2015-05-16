{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module XMLSerie where
import Serie (Serie(..))
import Text.XML.HXT.Core

instance XmlPickler Serie where
  xpickle = xpSerie



xpSerie :: PU Serie
xpSerie = xpElem "serie" $
          xpWrap ( \ (( d,e,m,o,t)) -> Serie d e m o t
                 , \ t -> (dir t, episode t, maxepisode t
                          , ongoing t, title t
                          )
                 ) $
          xp5Tuple (xpAttr "dir" xpickle)
                   (xpAttr "episode" xpickle)
                   (xpAttr "max" xpickle)
                   (xpAttr "ongoing" xpickle)
                   (xpAttr "title" xpickle)


-- getSerie = deep (isElem >>> hasName "serie") >>> 
--   proc x -> do
--     d <- getAttrValue "dir" -< x
--     e <- getAttrValue "episode" -< x
--     m <- getAttrValue "max" -< x
--     o <- getAttrValue "ongoing" -< x
--     t <- getText -< x
--     returnA -< Serie { dir = d, episode = e, maxepisode = m, ongoing = o, title = t}

processSerie	:: IOSArrow Serie Serie
processSerie
    = arrIO ( \ x -> do {print x ; return x})

          
runTest = 
  do
    runX ( xunpickleDocument xpSerie
           [ withValidate no
           , withTrace 1
           , withRemoveWS yes
           , withPreserveComment no
           ] "~/.serieviewer.xml"
           >>> processSerie >>>
           xpickleDocument   xpSerie
                               [ withIndent yes
                               ] "new-simple2.xml"
	   
         )
      return ()




    
    -- runX (readDocument [ withValidate no] "~/.serieviewer.xml" 
    --       >>> getSerie 
    --      )

