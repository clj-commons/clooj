(ns clooj.collaj
  (:import (java.net URLEncoder)))

(defn url-encode [s]
  (URLEncoder/encode s "UTF-8"))
  

(defn raw-data [terms]
  (read-string (slurp (str "http://collaj.net/data/"
                           (url-encode terms)))))


