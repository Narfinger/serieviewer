(ns serieviewer.routes.home
  (:require [serieviewer.layout :as layout]
            [serieviewer.serie :as S]
            [compojure.core :refer [defroutes GET]]
            [ring.util.http-response :refer [ok]]
            [clojure.java.io :as io]))

;; (def tmptestserie [(S/->Serie "Test title 1" 1 5 false),
;;                    (S/->Serie "test titlte 2" 2 5 false),
;;                    (S/->Serie "tttt  3" 2 6 true) ])


(defn home-page []
  (layout/render
   "home.html" {:series @S/series}))


(defn about-page []
  (layout/render "about.html"))

(defroutes home-routes
;  (let [series (S/parseXML "")]
    (GET "/" [] (home-page))
    (GET "/about" [] (about-page)));)

