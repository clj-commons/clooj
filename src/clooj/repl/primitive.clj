(ns clooj.repl.primitive
  (:import (java.net URL URLDecoder)
           (java.io File PrintWriter)
           (java.util.concurrent LinkedBlockingQueue))
  (:require [clojure.java.io :as io]
            [clooj.utils :as utils]
            [clooj.help :as help]
            [clj-inspector.jars :as jars]))
   
(defn own-clojure-jar
  "Locate the clojure jar being used by clooj (last resort)."
  []
  (let [class-loader (.getClassLoader clojure.lang.RT)]
    (when-let [url (.findResource class-loader "clojure/lang/RT.class")]
      (-> url .getFile URL. .getFile URLDecoder/decode (.split "!/") first))))

(defn jar-contains-class?
  "Does the jar contain a particular class file? Specify the
   classname in a string, e.g. \"clojure.lang.RT\""
  [jar classname]
  (let [entries (jars/get-entries-in-jar jar)
        filenames (map #(.getName %) entries)
        desired (str (.replace classname "." "/") ".class")]
    (not (nil? (some #(= % desired) filenames)))))

(defn clojure-jar-location
  "Find the location of a clojure jar in a project."
  [^String project-path]
  (let [lib-dir (str project-path "/lib")
        jars (filter #(.contains (.getName %) "clojure")
                     (jars/jar-files lib-dir))]
    (first (filter #(jar-contains-class? % "clojure.lang.RT") jars))))
        
(defn repl-classpath-pieces
  "Figures out the necessary pieces for a viable classpath
   given a particular project directory."
  [project-path]
  (set
    [(or (clojure-jar-location project-path)
         (own-clojure-jar))
     (str project-path "/lib/*")
     (str project-path "/src")]))
  
(defn java-binary
  "Returns the fully-qualified path of the java binary."
  []
  (str (System/getProperty "java.home")
       File/separator "bin" File/separator "java"))

(defn repl-process
  "Start a primitive repl process by running clojure.main."
  [project-path classpath]
  (let [classpath-str (apply str (interpose File/pathSeparatorChar classpath))]
    (.start
      (doto (ProcessBuilder. [(java-binary) "-cp" classpath-str "clojure.main"])
        (.redirectErrorStream true)
        (.directory (io/file (or project-path ".")))))))

(defn create-outside-repl
  "This function creates an outside process with a clojure repl."
  [project-path result-writer]
  (let [classpath (repl-classpath-pieces project-path)
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
    (future (utils/copy-input-stream-to-writer is result-writer)); :buffer-size 10))
    repl))
