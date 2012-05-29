(defproject clooj "0.3.4"
  :description "clooj, a small IDE for clojure"
  :main clooj.core
  :plugins [[lein-swank "1.4.4"]]
  :dependencies [[clojure "1.4.0"]
                 [clj-inspector "0.0.12"]
                 [slamhound "1.2.0"]
                 [com.cemerick/pomegranate "0.0.11"]
                 [com.fifesoft/rsyntaxtextarea "2.0.2"]
                 ]
)
