(defproject clooj "0.3.4.1-SNAPSHOT"
  :description "the start of a seesaw version of clooj, a small IDE for clojure"
  :main clooj.core
  :dependencies [[clojure "1.3.0"]
                 [clj-inspector "0.0.12"]
                 [slamhound "1.2.0"]
                 [com.cemerick/pomegranate "0.0.11"]
                 [com.fifesoft/rsyntaxtextarea "2.0.2"]
                 [seesaw "1.4.1"]]
  :jvm-opts ~(if (= (System/getProperty "os.name") "Mac OS X") ["-Xdock:name=Clooj"] [])
  :java-source-paths ["src"]
  :java-source-path "src"
  ;; Use this for Leiningen version 1
  :resources-path "resource"
  ;; Use this for Leiningen version 2
  :resource-paths ["resource"]
  )
