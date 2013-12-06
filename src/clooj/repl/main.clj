; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.repl.main
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
            [clojure.tools.nrepl :as nrepl]
            [clojure.java.io :as io]
            [clooj.brackets :as brackets]
            [clooj.help :as help]
            [clooj.project :as project]
            [clooj.repl.external :as external]
            [clooj.repl.lein :as lein]
            [clooj.protocols :as protocols]
            [clooj.utils :as utils]))

(use 'clojure.java.javadoc)

(def repl-history {:items (atom nil) :pos (atom 0)})

;; utils

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

(defn get-project-path [app]
  (when-let [repl (:repl app)]
    (-> repl deref :project-path)))

(defn initialize-repl [repl]
  (.evaluate repl
    (str    
      "(do"
      (utils/local-clj-source "clooj/cemerick/pomegranate.clj")
      (utils/local-clj-source "clooj/repl/remote.clj")    
      "(clooj.repl.remote/repl)"
      ")"
      )))

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
  [app cmd-str silent?]
  (when-let [repl @(app :repl)]
    (.evaluate repl
               (str (if silent?
                      "(clooj.repl.remote/silent"
                      "(do")
                    cmd-str ")"))))

(defn send-to-repl
  ([app cmd silent?] (send-to-repl app cmd "NO_SOURCE_PATH" 0 silent?))
  ([app cmd file line silent?]
      (let [cmd-ln (str (.trim cmd) \newline)
            cmd-trim (.trim cmd)
            classpaths (filter identity
                               (map #(.getAbsolutePath %)
                                    (-> app :classpath-queue)))
            ]
        (when-not silent?
          (utils/append-text (app :repl-out-text-area) cmd-ln))
        (let [cmd-str (cmd-attach-file-and-line cmd file line classpaths)]
          (print-to-repl app cmd-str silent?))
        (when-not silent?
          (when (not= cmd-trim (second @(:items repl-history)))
            (swap! (:items repl-history)
                   replace-first cmd-trim)
            (swap! (:items repl-history) conj ""))
          (reset! (:pos repl-history) 0)))))

(defn relative-file [app]
  (let [prefix (str (get-project-path app) File/separator
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
        (send-to-repl app txt (relative-file app) line false)))))

(defn send-doc-to-repl [app]
  (let [text (->> app :doc-text-area .getText)]
    (utils/append-text (app :repl-out-text-area) "Evaluating file...")
    (send-to-repl app text (relative-file app) 0 true)))

(defn make-repl-writer [ta-out]
  (->
    (proxy [Writer] []
      (write
        ([char-array offset length]
          ;(println "char array:" (apply str char-array) (count char-array))
          (utils/append-text ta-out (apply str char-array)))
        ([t]
          ;(println "t:" t)
          (if (= Integer (type t))
            (utils/append-text ta-out (str (char t)))
            (utils/append-text ta-out (apply str t)))))
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

(defn start-repl [app project-path]
  (let [project-path (if (utils/file-exists? project-path) project-path nil)]
    (utils/append-text (app :repl-out-text-area)
                       (str "\n=== Starting new REPL at " project-path " ===\n"))
    (let [classpath-items ;(lein/lein-classpath-items project-path)
          (external/repl-classpath-items project-path)
          repl ;(lein/lein-repl project-path (app :repl-out-writer))
          (external/repl project-path classpath-items 
                         (app :repl-out-writer))
          ]
      (initialize-repl repl)
      (help/update-var-maps! project-path classpath-items)
      (reset! (:repl app) repl))))

(defn stop-repl [app]
  (utils/append-text (app :repl-out-text-area)
                     "\n=== Shutting down REPL ===")
  (when-let [repl @(:repl app)]
    (.close repl)))

(defn apply-namespace-to-repl [app]
  (when-not @(:repl app)
    (start-repl app (first (project/get-selected-projects app))))
  (when-let [current-ns (get-file-ns app)]
    (send-to-repl app (str "(ns " current-ns ")") true)))

(defn restart-repl [app project-path]
  (stop-repl app)
  (start-repl app project-path)
  (apply-namespace-to-repl app))

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
                    (do (send-to-repl app txt false)
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
  (send-to-repl app "(when *e (.printStackTrace *e))" true))


  
