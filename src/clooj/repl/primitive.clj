(ns clooj.repl.primitive
  (:import (java.net URL URLDecoder)
           (java.io File PrintWriter)
           (java.util.concurrent LinkedBlockingQueue))
  (:require [clojure.java.io :as io]
            [clooj.utils :as utils]
            [clooj.help :as help]
            [clj-inspector.jars :as jars]))

(defn setup-classpath [project-path]
  (when project-path
    (let [project-dir (io/file project-path)]
      (when (and (.exists project-dir) (.isDirectory project-dir))
        (let [sub-dirs (utils/get-directories project-dir)]
          (concat sub-dirs
                  (filter #(.endsWith (.getName %) ".jar")
                          (mapcat #(.listFiles %) (file-seq project-dir)))))))))
   
(defn find-clojure-jar [class-loader]
  (when-let [url (.findResource class-loader "clojure/lang/RT.class")]
    (-> url .getFile URL. .getFile URLDecoder/decode (.split "!/") first)))

(defn clojure-jar-location
  "Find the location of a clojure jar in a project."
  [^String project-path]
  (let [lib-dir (str project-path "/lib")
        jars (filter #(.contains (.getName %) "clojure")
                     (jars/jar-files lib-dir))]
    (first
      (remove nil?
              (for [jar jars]
                (when-not
                  (empty?
                    (filter #(= "clojure/lang/RT.class" %)
                            (map #(.getName %) (jars/get-entries-in-jar jar))))
                  jar))))))
                       
        
(defn outside-repl-classpath [project-path]
  (let [clojure-jar-term (when-not (clojure-jar-location project-path)
                           (find-clojure-jar (.getClassLoader clojure.lang.RT)))]
    (filter identity [(str project-path "/lib/*")
                      (str project-path "/src")
                      (when clojure-jar-term
                        clojure-jar-term)])))

(defn copy-input-stream-to-writer [input-stream writer]
  (loop []
    (let [c (.read input-stream)]
      (when (not= c -1)
        (.write writer c)
        (recur)))))
  
(defn java-binary []
  (str (System/getProperty "java.home")
       File/separator "bin" File/separator "java"))

(defn repl-process [project-path classpath]
  (let [classpath-str (apply str (interpose File/pathSeparatorChar classpath))
        command [(java-binary) "-cp" classpath-str "clojure.main"]]
    ;(println command)
    (.start
      (doto (ProcessBuilder. command)
        (.redirectErrorStream true)
        (.directory (io/file (or project-path ".")))))))

(defn create-outside-repl
  "This function creates an outside process with a clojure repl."
  [result-writer project-path]
  (let [classpath (outside-repl-classpath project-path)
        proc (repl-process project-path classpath)
        input-writer  (-> proc .getOutputStream (PrintWriter. true))
        repl {:input-writer input-writer
              :project-path project-path
              :thread nil
              :proc proc
              :var-maps (agent nil)
              :classpath-queue (LinkedBlockingQueue.)}
        is (.getInputStream proc)]
    (send-off (repl :var-maps) #(merge % (help/get-var-maps project-path classpath)))
    (future (copy-input-stream-to-writer is result-writer)); :buffer-size 10))
    repl))