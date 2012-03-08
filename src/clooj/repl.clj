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
           (java.net URL URLClassLoader))
  (:use [clooj.utils :only (attach-child-action-keys attach-action-keys
                            awt-event
                            get-line-of-offset get-line-start-offset
                            append-text when-lets get-text-str get-directories)]
        [clooj.brackets :only (find-line-group find-enclosing-brackets)]
        [clojure.pprint :only (pprint)]
        [clooj.project :only (get-temp-file)])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(use 'clojure.java.javadoc)

(def repl-history {:items (atom nil) :pos (atom 0)})

(def repls (atom {}))

(def ^:dynamic *printStackTrace-on-error* false)

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
    (let [project-dir (File. project-path)]
      (when (and (.exists project-dir) (.isDirectory project-dir))
        (let [sub-dirs (get-directories project-dir)]
          (concat sub-dirs
                  (filter #(.endsWith (.getName %) ".jar")
                          (mapcat #(.listFiles %) (file-seq project-dir)))))))))

(defn selfish-class-loader [url-array parent]
  (proxy [URLClassLoader] [url-array nil]
    (findClass [classname]
      (try (proxy-super findClass classname)
           (catch ClassNotFoundException e
                  (.findClass parent classname))))))

(defn create-class-loader [project-path parent]
  (when project-path
    (let [files (setup-classpath project-path)
          urls (map #(.toURL %) files)]
      (println " Classpath:")
      (dorun (map #(println " " (.getAbsolutePath %)) files))
      (URLClassLoader.
        (into-array URL urls) parent
        ))))
    
(defn find-clojure-jar [class-loader]
  (when-let [url (.findResource class-loader "clojure/lang/RT.class")]
      (-> url .getFile URL. .getFile (.split "!/") first)))

(defn create-outside-repl
  "This function creates an outside process with a clojure repl."
  [result-writer project-path]
  (let [java (str (System/getProperty "java.home")
                  File/separator "bin" File/separator "java")
        builder (ProcessBuilder.
                  [java "-cp" (str "lib/*" File/pathSeparatorChar "src") "clojure.main"])]
    (.redirectErrorStream builder true)
    (.directory builder (File. (or project-path ".")))
    (let [proc (.start builder)
          input-writer  (-> proc .getOutputStream (PrintWriter. true))
          repl {:input-writer input-writer
                :project-path project-path
                :thread nil
                :proc proc}
          is (.getInputStream proc)]
      (future (io/copy is result-writer :buffer-size 1))
      (swap! repls assoc project-path repl)
      repl)))

(defn replace-first [coll x]
  (cons x (next coll)))

(defn update-repl-history [app]
  (swap! (:items repl-history) replace-first
         (get-text-str (app :repl-in-text-area))))

(defn correct-expression? [cmd]
  (when-not (empty? (.trim cmd))
    (let [rdr (-> cmd StringReader. PushbackReader.)]
      (try (while (read rdr nil nil))
           true
           (catch IllegalArgumentException e true) ;explicitly show duplicate keys etc.
           (catch Exception e false)))))

(defn read-string-at [source-text start-line]
  `(let [sr# (java.io.StringReader. ~source-text)
         rdr# (proxy [clojure.lang.LineNumberingPushbackReader] [sr#]
               (getLineNumber []
                              (+ ~start-line (proxy-super getLineNumber))))]
     (take-while #(not= % :EOF_REACHED)
                 (repeatedly #(try (read rdr#)
                                   (catch Exception e# :EOF_REACHED))))))

(defn cmd-attach-file-and-line [cmd file line]
  (let [read-string-code (read-string-at cmd line)]
    (pr-str
      `(binding [*file* ~file]
         (last
           (map eval ~read-string-code))))))
           
(defn send-to-repl
  ([app cmd] (send-to-repl app cmd "NO_SOURCE_PATH" 0))
  ([app cmd file line]
    (awt-event
      (let [cmd-ln (str \newline (.trim cmd) \newline)
            cmd-trim (.trim cmd)]
        (append-text (app :repl-out-text-area) cmd-ln)
        (binding [*out* (:input-writer @(app :repl))]
          (println (cmd-attach-file-and-line cmd file line))
          (flush))
        (when (not= cmd-trim (second @(:items repl-history)))
          (swap! (:items repl-history)
                 replace-first cmd-trim)
          (swap! (:items repl-history) conj ""))
        (reset! (:pos repl-history) 0)))))

(defn x []
  (java.lang.Exception. "Boo!"))

(defn scroll-to-last [text-area]
  (.scrollRectToVisible text-area
                        (Rectangle. 0 (.getHeight text-area) 1 1)))

(defn relative-file [app]
  (let [prefix (str (-> app :repl deref :project-path) File/separator
                    "src"  File/separator)]
    (when-lets [f @(app :file)
                path (.getAbsolutePath f)]
      (subs path (count prefix)))))

(defn selected-region [ta]
  (if-let [text (.getSelectedText ta)]
    {:text text
     :start (.getSelectionStart ta)
     :end   (.getSelectionEnd ta)}
    (let [[a b] (find-line-group ta)]
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
      (let [line (get-line-of-offset ta (:start region))]
        (send-to-repl app txt (relative-file app) line)))))

(defn send-doc-to-repl [app]
  (let [text (->> app :doc-text-area .getText)]
    (send-to-repl app text (relative-file app) 0)))

(defn repl-writer-write
  ([buffer char-array offset length]
    (.append buffer char-array offset length)
    buffer)
  ([buffer t]
    (when (= Long (type t))
      (.append buffer (char t)))
    buffer))

(defn repl-writer-flush [buf ta-out]
  (when ta-out
    (let [text (.toString buf)]
      (.setLength buf 0)
      (awt-event
        (do (append-text ta-out text)
            (scroll-to-last ta-out)))))
  buf)

(defn make-repl-writer [ta-out]
  (->
    (let [buf (agent (StringBuffer.))]
      (proxy [Writer] []
        (write
          ([char-array offset length]
            (awt-event (append-text ta-out (apply str char-array))))
          ([^Integer t]          
            (awt-event (append-text ta-out (str (char t))))))
        (flush [] nil)
        (close [] nil)))
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
  (when-lets [f0 @(:file app)
              f (or (get-temp-file f0) f0)]
    (send-to-repl app (str "(load-file \"" (.getAbsolutePath f) "\")"))))

(defn apply-namespace-to-repl [app]
  (when-let [current-ns (get-file-ns app)]
    (send-to-repl app (str "(ns " current-ns ")"))
    (swap! repls assoc-in
           [(-> app :repl deref :project-path) :ns]
           current-ns)))

(defn restart-repl [app project-path]
  (append-text (app :repl-out-text-area)
               (str "\n=== RESTARTING " project-path " REPL ===\n"))
  (let [input (-> app :repl deref :input-writer)]
    (.write input "EXIT-REPL\n")
    (.flush input))
  (Thread/sleep 100)
  (when-let [thread (-> app :repl deref :thread)]
    (while (.isAlive thread)
      (.stop thread)))
  (when-let [proc (-> app :repl deref :proc)]
    (.destroy proc))
  (reset! (:repl app) (create-outside-repl (app :repl-out-writer) project-path))
  (apply-namespace-to-repl app))

(defn switch-repl [app project-path]
  (when (and project-path
             (not= project-path (-> app :repl deref :project-path)))
    (append-text (app :repl-out-text-area)
                 (str "\n\n=== Switching to " project-path " REPL ===\n"))
    (let [repl (or (get @repls project-path)
                   (create-outside-repl (app :repl-out-writer) project-path))]
      (reset! (:repl app) repl))))

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
                   (= -1 (first (find-enclosing-brackets
                                  txt
                                  caret-pos)))))
        submit #(when-let [txt (.getText ta-in)]
                  (if (correct-expression? txt)
                    (do (send-to-repl app txt)
                        (.setText ta-in ""))
                    (.setText (app :arglist-label) "Malformed expression")))
        at-top #(zero? (get-line-of-offset ta-in (get-caret-pos)))
        at-bottom #(= (get-line-of-offset ta-in (get-caret-pos))
                      (get-line-of-offset ta-in (.. ta-in getText length)))
        prev-hist #(show-previous-repl-entry app)
        next-hist #(show-next-repl-entry app)]
    (attach-child-action-keys ta-in ["UP" at-top prev-hist]
                              ["DOWN" at-bottom next-hist]
                              ["ENTER" ready submit])
    (attach-action-keys ta-in ["cmd1 UP" prev-hist]
                        ["cmd1 DOWN" next-hist]
                        ["cmd1 ENTER" submit])))

(defn print-stack-trace [app]
    (send-to-repl app "(.printStackTrace *e)"))
