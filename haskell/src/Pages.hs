{-# LANGUAGE OverloadedStrings #-}

module Pages
       ( indexPage
       )
       where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

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
  , H.script ! A.src "static/js/bootstrap.min.js" $ ""
  ] body


indexPage :: H.Html
indexPage = siteTemplate "Main" "TestSeries"
