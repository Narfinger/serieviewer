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
  (if (.exists (clojure.java.io/as-file filename))
    (let [xmldoc (xml/parse filename)]
      (map parseSerie (filter (fn [elt] (= :serie (:tag elt))) (xml-seq xmldoc))))
    nil))
      

(defonce ^:dynamic series  (atom (parseXML "serieviewer.xml")))
