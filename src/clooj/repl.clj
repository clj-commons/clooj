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
           (java.awt Rectangle)
           (java.net URL URLClassLoader))
  (:use [clooj.utils :only (attach-child-action-keys attach-action-keys
                            awt-event recording-source-reader
                            get-line-of-offset get-line-start-offset
                            append-text when-lets)]
        [clooj.brackets :only (find-line-group find-enclosing-brackets)]
        [clojure.pprint :only (pprint)]
        [clooj.project :only (get-temp-file)])
  (:require [clojure.string :as string]))

(def repl-history {:items (atom nil) :pos (atom 0)})

(def repls (atom {}))

(def ^:dynamic *printStackTrace-on-error* false)

(def ^:dynamic *line-offset*)

(defn is-eof-ex? [throwable]
  (and (instance? clojure.lang.LispReader$ReaderException throwable)
       (or
         (.startsWith (.getMessage throwable) "java.lang.Exception: EOF while reading")
         (.startsWith (.getMessage throwable) "java.io.IOException: Write end dead"))))

(defn get-repl-ns [app]
  (let [repl-map @repls]
    (-> app :repl deref :project-path repl-map :ns)))

(defn repl-print [x]
  (if (var? x)
    (print x)
    (pprint x)))

(defn setup-classpath [project-path]
  (when project-path
    (let [project-dir (File. project-path)]
      (when (and (.exists project-dir) (.isDirectory project-dir))
        (let [sub-dirs (filter #(and (.isDirectory %)
                                     (not (.startsWith (.getName %) ".")))
                               (.listFiles project-dir))]
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
        (into-array URL urls)
        ))))
    
(defn find-clojure-jar [class-loader]
  (when-let [url (.findResource class-loader "clojure/lang/RT.class")]
      (-> url .getFile URL. .getFile (.split "!/") first)))

(defn create-outside-repl
  "This function creates an outside process with a clojure repl."
  [project-path classpath]
  (let [java (str (System/getProperty "java.home")
                  File/separator "bin" File/separator "java")
        builder (ProcessBuilder. [java "-cp" classpath "clojure.main"])]
    (.redirectErrorStream builder true)
    (.directory builder project-path)
    (let [proc (.start builder)
          reader #(-> % (InputStreamReader. "utf-8") BufferedReader.)]
      {:in (-> proc .getOutputStream (PrintWriter. true))
       :out (-> proc .getInputStream reader)
       :err (-> proc .getErrorStream reader)})))

(defn transmit [reader-in writer-out]
  (doto (Thread. (while true
                   (.println writer-out
                             (.readLine reader-in)))) .start))  

(defn create-clojure-repl
  "This function creates an instance of clojure repl, with output going to output-writer
   Returns an input writer."
  [result-writer project-path]
  (let [classloader (create-class-loader project-path
                                         (.getClassLoader clojure.lang.RT))
        first-prompt (atom true)
        input-writer (PipedWriter.)
        piped-in (-> input-writer
                     PipedReader.
                     (recording-source-reader (var *line-offset*)))
        repl-thread-fn #(binding [*printStackTrace-on-error* *printStackTrace-on-error*
                                  *in* piped-in
                                  *out* result-writer
                                  *err* *out*
                                  *file* "NO_SOURCE_PATH"
                                  *line-offset* 0]
                          (try
                            (clojure.main/repl
                              :init (fn [] (in-ns 'user))
                              :print (fn [& args]
                                       (dorun (map repl-print args)))
                              :read (fn [prompt exit]
                                      (let [form (read)]
                                        (cond
                                          (= form 'EXIT-REPL)
                                           exit
                                          (= form 'SILENT-EVAL)
                                           (do (eval (read))
                                               (.toString piped-in)
                                               (recur prompt exit))
                                          (isa? (type form) clojure.lang.IObj)
                                           (with-meta form
                                                      (assoc (meta form)
                                                             :clooj/src (.toString piped-in)))
                                          :else
                                           form)))
                              :caught (fn [e]
                                        (when (is-eof-ex? e)
                                          (throw e))
                                        (if *printStackTrace-on-error*
                                          (.printStackTrace e *out*)
                                          (prn (clojure.main/repl-exception e))))
                              :prompt (fn []
                                        (printf "\n%s=>"
                                                (ns-name *ns*)))
                              :need-prompt (constantly true)
                              :eval (fn [form]
                                      (let [val (eval form)]
                                        (when (var? val)
                                          (alter-meta! val
                                                       (fn [v] (merge (meta form) v))))
                                        val))
                              )
                            (catch clojure.lang.LispReader$ReaderException ex
                                   (prn (.getCause ex))
                                   (prn "REPL closing"))
                            (catch java.lang.InterruptedException ex)
                            (catch java.nio.channels.ClosedByInterruptException ex)))
        thread (Thread. repl-thread-fn)]
    (when classloader
      (doto thread
        (.setContextClassLoader classloader)   
        .start))
    (let [repl {:thread thread
                :input-writer input-writer
                :project-path project-path}]
      (swap! repls assoc project-path repl)
      repl)))

(defn replace-first [coll x]
  (cons x (next coll)))

(defn update-repl-history [app]
  (swap! (:items repl-history) replace-first
         (.getText (app :repl-in-text-area))))

(defn correct-expression? [cmd]
  (let [rdr (-> cmd StringReader. PushbackReader.)]
    (try (while (read rdr nil nil))
         true
         (catch IllegalArgumentException e true) ;explicitly show duplicate keys etc.
         (catch Exception e false))))

(defn send-to-repl
  ([app cmd] (send-to-repl app cmd "NO_SOURCE_PATH" 1))
  ([app cmd file line]
    (awt-event
      (let [cmd-ln (str \newline (.trim cmd) \newline)
            cmd (.trim cmd-ln)]
        (append-text (app :repl-out-text-area) cmd-ln)
        (binding [*out* (:input-writer @(app :repl))]
            (pr 'SILENT-EVAL `(set! *file* ~file)
                'SILENT-EVAL `(set! *line-offset*
                                    (+ *line-offset*
                                       (- ~line (.getLineNumber *in*)))))
          (println cmd)
          (flush))
        (when (not= cmd (second @(:items repl-history)))
          (swap! (:items repl-history)
                 replace-first cmd)
          (swap! (:items repl-history) conj ""))
        (reset! (:pos repl-history) 0)))))

(defn scroll-to-last [text-area]
  (.scrollRectToVisible text-area
                        (Rectangle. 0 (.getHeight text-area) 1 1)))

(defn relative-file [app]
  (let [prefix (str (-> app :repl deref :project-path) File/separator
                    "src"  File/separator)]
    (when-let [path (.getAbsolutePath @(app :file))]
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
        (send-to-repl app txt (relative-file app) (inc line))))))

(defn send-doc-to-repl [app]
  (let [text (->> app :doc-text-area .getText)]
    (send-to-repl app text (relative-file app) 1)))

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
            (send-off buf repl-writer-write char-array offset length))
          ([t]          
            (send-off buf repl-writer-write t)))
        (flush [] (send-off buf repl-writer-flush ta-out))
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
         #(Math/min (dec (count @(:items repl-history))) (inc %)))
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
  (let [thread (-> app :repl deref :thread)]
    (while (.isAlive thread)
      (.stop thread)))
  (reset! (:repl app) (create-clojure-repl (app :repl-out-writer) project-path))
  (apply-namespace-to-repl app))

(defn switch-repl [app project-path]
  (when (and project-path
             (not= project-path (-> app :repl deref :project-path)))
    (append-text (app :repl-out-text-area)
                 (str "\n\n=== Switching to " project-path " REPL ===\n"))
    (let [repl (or (get @repls project-path)
                   (create-clojure-repl (app :repl-out-writer) project-path))]
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

