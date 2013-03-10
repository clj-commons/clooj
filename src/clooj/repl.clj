; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.repl
  (:import (java.io
             BufferedReader BufferedWriter
             InputStreamReader
             File PipedReader PipedWriter PrintWriter Writer
                    StringReader PushbackReader)
           (clojure.lang LineNumberingPushbackReader)
           (java.awt Rectangle)
           (java.net URL URLClassLoader URLDecoder)
           (java.util.concurrent LinkedBlockingQueue))
  (:require [clj-inspector.jars :as jars]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clooj.brackets :as brackets]
            [clooj.help :as help]
            [clooj.project :as project]
            [clooj.utils :as utils]))

(use 'clojure.java.javadoc)

(def repl-history {:items (atom nil) :pos (atom 0)})

(def repls (atom {}))

(def ^:dynamic *printStackTrace-on-error* false)

(defn tokens
  "Finds all the tokens in a given string."
  [text]
  (re-seq #"[\w\d/\-\.\?\+\!\*\$\>\<]+" text))

(defn namespaces-from-code
  "Take tokens from text and extract namespace symbols."
  [text]
  (->> text tokens (filter #(.contains % "/"))
       (map #(.split % "/"))
       (map first)
       (map #(when-not (empty? %) (symbol %)))
       (remove nil?)))

(defn is-eof-ex? [throwable]
  (and (instance? clojure.lang.LispReader$ReaderException throwable)
       (or
         (.startsWith (.getMessage throwable) "java.lang.Exception: EOF while reading")
         (.startsWith (.getMessage throwable) "java.io.IOException: Write end dead"))))

(defn get-repl-ns [app]
  (let [repl-map @repls]
    (-> app :repl deref :project-path repl-map :ns)))

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

(defn load-pomegranate-stub []
  (utils/local-clj-source "clooj/cemerick/pomegranate.clj"))

(defn initialize-repl [repl-input-writer current-ns]
  (binding [*out* repl-input-writer]
    (print "(clojure.main/repl
            :print (fn [x]
                     (if (var? x)
                       (println x)
                       (clojure.pprint/pprint x)))
            :prompt #(do (clojure.main/repl-prompt) (.flush *out*)))"
           "(do "
             ;(set! *print-length* 20)"
             (load-pomegranate-stub) 
           "(ns " current-ns "))"
           )))

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
    (println command)
    (.start
      (doto (ProcessBuilder. command)
        (.redirectErrorStream true)
        (.directory (io/file (or project-path ".")))))))

(defn create-outside-repl
  "This function creates an outside process with a clojure repl."
  [result-writer project-path ns]
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
    (swap! repls assoc project-path repl)
    (initialize-repl input-writer ns)
    repl))

(defn replace-first [coll x]
  (cons x (next coll)))

(defn update-repl-history [app]
  (swap! (:items repl-history) replace-first
         (utils/get-text-str (app :repl-in-text-area))))

(defn correct-expression? [cmd]
  (when-not (empty? (.trim cmd))
    (let [rdr (-> cmd StringReader. PushbackReader.)]
      (try (while (read rdr nil nil))
           true
           (catch IllegalArgumentException e true) ;explicitly show duplicate keys etc.
           (catch Exception e false)))))

(defn read-string-at [source-text start-line]
  `(let [sr# (java.io.StringReader. (str (apply str (repeat ~start-line "\n"))
                                         ~source-text))
         rdr# (clojure.lang.LineNumberingPushbackReader. sr#)]
     (take-while #(not= % :EOF_REACHED)
                 (repeatedly #(try (read rdr#)
                                   (catch Exception e# :EOF_REACHED))))))

(defn cmd-attach-file-and-line [cmd file line classpaths]
  (let [read-string-code (read-string-at cmd line)
        short-file (last (.split file "/"))
        namespaces (namespaces-from-code cmd)]
    ;(println namespaces)
    (pr-str
      `(do
         (dorun (map #(try (clooj.cemerick.pomegranate/add-classpath %)
                           (catch Exception e# (println e#))) '~classpaths))
         (dorun (map #(try (require %) (catch Exception _#)) '~namespaces))
         (binding [*source-path* ~short-file
                   *file* ~file]
           (last (map eval ~read-string-code)))))))
           
(defn print-to-repl
  [app cmd-str]
  ;(println @(app :repl))
  (binding [*out* (:input-writer @(app :repl))]
    (println cmd-str)
    (flush)))

(defn drain-queue 
  [queue]
  (let [array-list (java.util.ArrayList.)]
    (.drainTo queue array-list)
    (seq array-list)))

(defn send-to-repl
  ([app cmd] (send-to-repl app cmd "NO_SOURCE_PATH" 0))
  ([app cmd file line]
    (utils/awt-event
      (let [cmd-ln (str \newline (.trim cmd) \newline)
            cmd-trim (.trim cmd)
            classpaths (filter identity
                               (map #(.getAbsolutePath %)
                                    (-> app :repl deref :classpath-queue drain-queue)))]
        (utils/append-text (app :repl-out-text-area) cmd-ln)
        (let [cmd-str (cmd-attach-file-and-line cmd file line classpaths)]
          (print-to-repl app cmd-str))
        (when (not= cmd-trim (second @(:items repl-history)))
          (swap! (:items repl-history)
                 replace-first cmd-trim)
          (swap! (:items repl-history) conj ""))
        (reset! (:pos repl-history) 0)))))

(defn scroll-to-last [text-area]
  (.scrollRectToVisible text-area
                        (Rectangle. 0 (dec (.getHeight text-area)) 1 1)))

(defn relative-file [app]
  (let [prefix (str (-> app :repl deref :project-path) File/separator
                    "src"  File/separator)]
    (utils/when-lets [f @(app :file)
                      path (.getAbsolutePath f)]
      (subs path (count prefix)))))

(defn selected-region [ta]
  (if-let [text (.getSelectedText ta)]
    {:text text
     :start (.getSelectionStart ta)
     :end   (.getSelectionEnd ta)}
    (let [[a b] (brackets/find-line-group ta)]
      (when (and a b (< a b))
        {:text (.. ta getDocument (getText a (- b a)))
         :start a
         :end b}))))

(defn send-selected-to-repl [app]
  (let [ta (app :doc-text-area)
        region (selected-region ta)
        txt (:text region)]
    (if-not (and txt (correct-expression? txt))
      (.setText (app :arglist-label) "Malformed expression")
      (let [line (.getLineOfOffset ta (:start region))]
        (send-to-repl app txt (relative-file app) line)))))

(defn send-doc-to-repl [app]
  (let [text (->> app :doc-text-area .getText)]
    (send-to-repl app text (relative-file app) 0)))

(defn make-repl-writer [ta-out]
  (->
    (proxy [Writer] []
      (write
        ([char-array offset length]
          ;(println "char array:" (apply str char-array) (count char-array))
          (utils/awt-event (utils/append-text ta-out (apply str char-array))))
        ([t]
          (if (= Integer (type t))
            (utils/awt-event (utils/append-text ta-out (str (char t))))
            (utils/awt-event (utils/append-text ta-out (apply str t))))))
      (flush [])
      (close [] nil))
    (PrintWriter. true)))
  
(defn update-repl-in [app]
  (when (pos? (count @(:items repl-history)))
    (.setText (:repl-in-text-area app)
              (nth @(:items repl-history) @(:pos repl-history)))))

(defn show-previous-repl-entry [app]
  (when (zero? @(:pos repl-history))
    (update-repl-history app))
  (swap! (:pos repl-history)
         #(min (dec (count @(:items repl-history))) (inc %)))
  (update-repl-in app))

(defn show-next-repl-entry [app]
  (when (pos? @(:pos repl-history))
    (swap! (:pos repl-history)
           #(Math/max 0 (dec %)))
    (update-repl-in app)))

(defn get-file-ns [app]
  (try
    (when-let [sexpr (read-string (.getText (app :doc-text-area)))]
      (when (= 'ns (first sexpr))
        (str (second sexpr))))
    (catch Exception e)))

(defn load-file-in-repl [app]
  (utils/when-lets [f0 @(:file app)
                    f (or (project/get-temp-file f0) f0)]
    (send-to-repl app (str "(load-file \"" (.getAbsolutePath f) "\")"))))

(defn apply-namespace-to-repl [app]
  (when-let [current-ns (get-file-ns app)]
    (send-to-repl app (str "(ns " current-ns ")"))
    (swap! repls assoc-in
           [(-> app :repl deref :project-path) :ns]
           current-ns)))
  
(defn restart-repl [app project-path]
  (utils/awt-event (utils/append-text (app :repl-out-text-area)
                                (str "\n=== RESTARTING " project-path " REPL ===\n")))
  (when-let [proc (-> app :repl deref :proc)]
    (.destroy proc))
  (reset! (:repl app) (create-outside-repl (app :repl-out-writer) project-path (get-file-ns app)))
  (apply-namespace-to-repl app))

(defn switch-repl [app project-path]
  (when (and project-path
             (not= project-path (-> app :repl deref :project-path)))
    (utils/awt-event
      (utils/append-text (app :repl-out-text-area)
                   (str "\n\n=== Switching to " project-path " REPL ===\n")))
    (let [repl (or (get @repls project-path)
                   (create-outside-repl (app :repl-out-writer) project-path (get-file-ns app)))]
      (reset! (:repl app) repl)
      ;(apply-namespace-to-repl app)
      )))

(defn add-repl-input-handler [app]
  (let [ta-in (app :repl-in-text-area)
        get-caret-pos #(.getCaretPosition ta-in)
        ready #(let [caret-pos (get-caret-pos)
                     txt (.getText ta-in)
                     trim-txt (string/trimr txt)]
                 (and
                   (pos? (.length trim-txt))
                   (<= (.length trim-txt)
                       caret-pos)
                   (= -1 (first (brackets/find-enclosing-brackets
                                  txt
                                  caret-pos)))))
        submit #(when-let [txt (.getText ta-in)]
                  (if (correct-expression? txt)
                    (do (send-to-repl app txt)
                        (.setText ta-in ""))
                    (.setText (app :arglist-label) "Malformed expression")))
        at-top #(zero? (.getLineOfOffset ta-in (get-caret-pos)))
        at-bottom #(= (.getLineOfOffset ta-in (get-caret-pos))
                      (.getLineOfOffset ta-in (.. ta-in getText length)))
        prev-hist #(show-previous-repl-entry app)
        next-hist #(show-next-repl-entry app)]
    (utils/attach-child-action-keys ta-in ["UP" at-top prev-hist]
                              ["DOWN" at-bottom next-hist]
                              ["ENTER" ready submit])
    (utils/attach-action-keys ta-in ["cmd1 UP" prev-hist]
                        ["cmd1 DOWN" next-hist]
                        ["cmd1 ENTER" submit])))

(defn print-stack-trace [app]
    (send-to-repl app "(.printStackTrace *e)"))

    
  
