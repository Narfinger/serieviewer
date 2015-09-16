(ns serieviewer.routes.home
  (:require [serieviewer.layout :as layout]
            [serieviewer.serie :as S]
            [compojure.core :refer [defroutes GET]]
            [ring.util.http-response :refer [ok]]
            [clojure.java.io :as io]))

;; (def tmptestserie [(S/->Serie "Test title 1" 1 5 false),
;;                    (S/->Serie "test titlte 2" 2 5 false),
;;                    (S/->Serie "tttt  3" 2 6 true) ])

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.
  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))


(defn home-page []
  (let [list (indexed @S/series)]
    (layout/render
     "home.html" {:series list})))


(defn about-page []
  (layout/render "about.html"))

(defroutes home-routes
;  (let [series (S/parseXML "")]
    (GET "/" [] (home-page))
    (GET "/about" [] (about-page)));)

