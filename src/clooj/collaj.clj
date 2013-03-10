; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.collaj
  (:import (java.net URLEncoder)))

(defn url-encode
  "URL-encode a string."
  [s]
  (URLEncoder/encode s "UTF-8"))
  
(defn raw-data
  "Get a clojure data collection of raw search
   results from collaj.net"
  [terms]
  (read-string (slurp (str "http://collaj.net/?format=raw&q="
                           (url-encode terms)))))

