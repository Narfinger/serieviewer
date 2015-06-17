{-# LANGUAGE OverloadedStrings,CPP, FlexibleInstances, GeneralizedNewtypeDeriving, QuasiQuotes #-}
module Pages
       ( indexPage
       , playPage
       , modifyPage
       )
       where
import Control.Concurrent.STM
import Control.Monad (forM_)
import Data.UUID
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Language.Javascript.JMacro ( ToJExpr(..), Ident(..), renderJs
                                  , JStat(..), JExpr(..)
                                  , JVal(..), jmacro, jsv
                                  , jLam, jVarTy)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.Text.Lazy (unpack)
import Text.PrettyPrint.Leijen.Text (renderOneLine, displayT)

import qualified Bootstrap as B
import Serie (Serie(..))
import Utils (replaceJSQuotes)


mainTemplate :: String -> [H.Html] -> H.Html -> H.Html
mainTemplate title headers body =
  H.docTypeHtml $ do
    H.head $ do
      H.title (H.toHtml title)
      sequence_ headers
    H.body $ do
      body

siteTemplate :: String -> H.Html -> H.Html
siteTemplate title body =
  mainTemplate title
  [
    H.link ! A.rel "stylesheet" ! A.href "static/css/bootstrap.min.css"
  , H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js" $ ""
  , H.script ! A.src "static/js/bootstrap.min.js" $ ""
  ] body

--jsToAttribute :: Language.Javascript.JMacro.Base.JsToDoc a, Language.Javascript.JMacro.Base.JMacro a) => a -> H.AttributeValue
-- renderJs gives us a type Doc which we transform to SimpleDoc without newlines, put it into a lazy text and unpack it to string
jsToAttribute js = H.preEscapedToValue $ replaceJSQuotes $ filter (/='\\') $ unpack $ displayT $ renderOneLine $ renderJs js

buildJsPostRequest :: (Show a) => String -> a -> H.AttributeValue
buildJsPostRequest s i =
  let execstring = s ++ show i ++ "/" in
   jsToAttribute [jmacro|$.post( `(execstring)` ); |]

playButton :: Int -> H.Html
playButton i =
  let exec = buildJsPostRequest "/execute/play/" i in
   H.button ! A.type_ "submit" ! A.class_ "btn btn-success" ! A.onclick exec $ do
     "Play"

modifyButton :: Int -> H.Html
modifyButton i =
  let exec = jsToAttribute ([jmacro| window.location="/modify/"; |]) in
  H.button ! A.type_ "submit" ! A.class_ "btn btn-info" ! A.onclick exec $ do
    "Modify"

serieSpinBox :: (Serie, Int) -> H.Html
serieSpinBox (s,i) =
  let maxvalue = H.stringValue $ show $ maxepisode s
      e =  H.stringValue $ show $ episode s
      exec = buildJsPostRequest "/execute/change/" i in
   H.td $ H.input ! A.type_ "number" ! A.step "1" ! A.min "1" ! A.max  maxvalue ! A.value e ! A.onchange exec

serieRow :: (Serie,Int) -> H.Html
serieRow (s,i) =
  H.tr $ do
    H.td $ H.toHtml $ title s
    serieSpinBox (s,i)
    H.td $ H.toHtml $ maxepisode s
    H.td $ H.toHtml $ ongoing s
    H.td $ playButton i
    H.td $ modifyButton i

playPage :: Serie -> H.Html
playPage serie = siteTemplate "TMP" "blubber"   
  

indexPage :: [Serie] -> H.Html
indexPage series =
  let zipped = zip series [0..] in
   siteTemplate "Main" $
     H.div ! A.class_ "container" $ do
       H.div ! A.class_ "row" $ do
         H.h1 $ "Series"
         H.form $ do
           H.table ! A.class_ "table table-striped" $ do
             H.tr $ do
               H.td "Title"
               H.td "Episode"
               H.td "Max Episode"
               H.td "Ongoing"
               H.td "Play"
               H.td "Modify"
             forM_ zipped serieRow


modifyPage :: H.Html
modifyPage =
  siteTemplate "Main" $
  H.h1 $ "BLUBBER"

stuff = Serie { dir = "/tmp", episode = 3, maxepisode = 10, ongoing = False, title = "Test this twice", uuid = nil}
teststuff =
  let testseries = [ Serie { dir = "/tmp", episode = 1, maxepisode = 5,  ongoing = False, title = "Test this onece", uuid = nil}
             , Serie { dir = "/tmp", episode = 3, maxepisode = 10, ongoing = False, title = "Test this twice",  uuid = nil}
             , Serie { dir = "/tmp", episode = 1, maxepisode = 3,  ongoing = True,  title = "Test this thrice",  uuid = nil}]
      stuff = [Serie { dir = "/tmp", episode = 3, maxepisode = 10, ongoing = False, title = "Test this twice", uuid = nil}]
  in
   renderHtml $  serieSpinBox (stuff !! 0, 1)
