(ns serieviewer.serie
  (:require [clojure.xml :as xml]
            [clojure.core]))

(defrecord Serie [title dir episode maxepisode ongoing])

(defn- parseSerie [xmlSerie]
  (let [attributes (:attrs xmlSerie)]
        (let [title (first (:content xmlSerie))
              episode (Integer. (:episode attributes))
              maxepisode (Integer. (:max attributes))
              ongoing (= "true" (:ongoing attributes))
              dir (:dir attributes)]
    (Serie. title dir episode maxepisode ongoing))))
                           
(defn- parseXML [filename]
  (let [xmldoc (xml/parse filename)]
    (map parseSerie (filter (fn [elt] (= :serie (:tag elt))) (xml-seq xmldoc)))))
      

(defonce ^:dynamic series  (atom (parseXML "serieviewer.xml")))
