; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.repl
  (:import (java.io File PipedReader PipedWriter PrintWriter Writer
                    StringReader PushbackReader)
           (java.awt Rectangle)
           (java.util.concurrent LinkedBlockingQueue)
           (java.net URL URLClassLoader))
  (:use [clooj.utils :only (attach-child-action-keys attach-action-keys
                            awt-event recording-source-reader
                            get-line-of-offset get-line-start-offset
                            append-text)]
        [clooj.brackets :only (find-line-group find-enclosing-brackets)]
        [clooj.project :only (get-selected-file-path)]
        [clojure.pprint :only (pprint)])
  (:require [clojure.contrib.string :as string]))

(def repl-history {:items (atom nil) :pos (atom 0)})

(def repls (atom {}))

(def *printStackTrace-on-error* false)

(defn is-eof-ex? [throwable]
  (and (instance? clojure.lang.LispReader$ReaderException throwable)
  (or
    (.startsWith (.getMessage throwable) "java.lang.Exception: EOF while reading")
    (.startsWith (.getMessage throwable) "java.io.IOException: Write end dead"))))

(defn get-repl-ns [doc]
  (let [repl-map @repls]
    (-> doc :repl deref :project-path repl-map :ns)))

(defn repl-print [x]
  (if (var? x)
    (print x)
    (pprint x)))

(defn get-lib-dirs [project-path]
  (when project-path
    (list (File. project-path "lib")
          (File. project-path "jars"))))

(defn create-class-loader [project-path]
  (when-let [lib-dirs (get-lib-dirs project-path)]
    (let [files (conj (apply concat (map #(.listFiles %) lib-dirs)) (File. project-path "src"))
          urls (map #(.toURL %) files)]
      (URLClassLoader.
        (into-array URL urls)
        (.getClassLoader clojure.lang.RT)))))
    
(defn repl-read [form-queue prompt exit]
  (let [form (.take form-queue)]
    (if (= form 'EXIT-REPL)
      exit
      form))) 

(defn repl-eval [form]
  (let [val (eval form)]
    (when (var? val)
      (alter-meta! val merge
        (:clooj/source (meta form))))
    val))

(defn create-clojure-repl
  "This function creates an instance of clojure repl, with output going to output-writer
  Returns an input writer."
  [result-writer project-path]
  (let [classloader (create-class-loader project-path)
        first-prompt (atom true)
        form-queue (LinkedBlockingQueue.)
        repl-thread-fn #(binding [*printStackTrace-on-error* *printStackTrace-on-error*
                                  *out* result-writer
                                  *err* *out*]
               (try
                 (clojure.main/repl
                   :init (fn [] (in-ns 'user))
                   :print (fn [& args]
                            (dorun (map repl-print args)))
                   :read (fn [prompt exit]
                           (repl-read form-queue prompt exit))
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
                   :eval (fn [form] (repl-eval form))
                   )
                 (catch clojure.lang.LispReader$ReaderException ex
                   (prn (.getCause ex))
                   (prn "REPL closing"))
                 (catch java.lang.InterruptedException ex)
                 (catch java.nio.channels.ClosedByInterruptException ex)))
        thread (Thread. repl-thread-fn)]
    (when classloader
      (.setContextClassLoader thread classloader))
    (.start thread)
    (let [repl {:thread thread
                :form-queue form-queue
                :project-path project-path}]
      (swap! repls assoc project-path repl)
      repl)))

(defn replace-first [coll x]
  (cons x (next coll)))

(defn update-repl-history [doc]
  (swap! (:items repl-history) replace-first
         (.getText (doc :repl-in-text-area))))

(defn correct-expression? [cmd]
   (let [rdr (-> cmd StringReader. PushbackReader.)]
        (try (while (read rdr nil nil))
          true
          (catch Exception e false))))

(defn send-to-repl [doc cmd-str meta-map]
  (awt-event
    (let [cmd (.trim cmd-str)
          cmd-ln (str \newline cmd \newline)]
      (append-text (doc :repl-out-text-area) cmd-ln)
      (let [bare-form (read-string cmd)
            form (if (isa? (type bare-form) clojure.lang.IObj)
                   (with-meta bare-form meta-map)
                   bare-form)]
       ; (println meta-map "|" bare-form "|" form "|" (meta form))
        (.put (:form-queue @(doc :repl)) form))
      (when (not= cmd (second @(:items repl-history)))
        (swap! (:items repl-history)
               replace-first cmd)
        (swap! (:items repl-history) conj ""))
       (reset! (:pos repl-history) 0))))

(defn scroll-to-last [text-area]
  (.scrollRectToVisible text-area
    (Rectangle. 0 (.getHeight text-area) 1 1)))


(defn send-selected-to-repl [doc]
  (let [ta (doc :doc-text-area)
        [a b] (find-line-group ta)
        txt (or
              (.getSelectedText ta)
                (when (and a b (< a b))
                  (.. ta getDocument
                    (getText a (- b a)))))]     
    (if-not (and txt (correct-expression? txt))
      (.setText (doc :arglist-label) "Malformed expression")
      (send-to-repl doc txt
                    {:clooj/source
                       {:line (inc (get-line-of-offset ta a))
                        :file (get-selected-file-path doc)}}))))

(defn send-doc-to-repl [doc]
  (let [txt (->> doc :doc-text-area .getText)]
    (send-to-repl doc txt nil)))

(defn make-repl-writer [ta-out]
  (->
    (let [buf (StringBuffer.)]
      (proxy [Writer] []
        (write
          ([char-array offset length]
            (.append buf char-array offset length))
          ([^int t]          
            (when (= Integer (type t))
              (.append buf (char t)))))
        (flush []
          (when ta-out
            (awt-event
              (do (append-text ta-out (.toString buf))
                  (scroll-to-last ta-out)
                  (.setLength buf 0)))))
        (close [] nil)))
    (PrintWriter. true)))
  
(defn update-repl-in [doc]
  (when (pos? (count @(:items repl-history)))
    (.setText (:repl-in-text-area doc)
      (nth @(:items repl-history) @(:pos repl-history)))))

(defn show-previous-repl-entry [doc]
  (when (zero? @(:pos repl-history))
        (update-repl-history doc))
  (swap! (:pos repl-history)
         #(Math/min (dec (count @(:items repl-history))) (inc %)))
  (update-repl-in doc))

(defn show-next-repl-entry [doc]
  (when (pos? @(:pos repl-history))
    (swap! (:pos repl-history)
           #(Math/max 0 (dec %)))
    (update-repl-in doc)))
  
(defn get-current-namespace [text-comp]
  (try
    (when-let [sexpr (read-string (. text-comp getText))]
      (when (= 'ns (first sexpr))
        (str (second sexpr))))
    (catch Exception e)))

(defn apply-namespace-to-repl [doc]
  (when-let [current-ns (get-current-namespace (doc :doc-text-area))]
    (send-to-repl doc (str "(ns " current-ns ")") nil)
    (swap! repls assoc-in
           [(-> doc :repl deref :project-path) :ns]
           current-ns)))

(defn restart-repl [doc project-path]
  (append-text (doc :repl-out-text-area)
           (str "\n=== RESTARTING " project-path " REPL ===\n"))
  (let [input (-> doc :repl deref :form-queue)]
    (.put input ['EXIT-REPL nil]))
  (Thread/sleep 100)
  (let [thread (-> doc :repl deref :thread)]
    (while (.isAlive thread)
      (.stop thread)))
  (reset! (:repl doc) (create-clojure-repl (doc :repl-out-writer) project-path))
  (apply-namespace-to-repl doc))

(defn switch-repl [doc project-path]
  (when (not= project-path (-> doc :repl deref :project-path))
    (append-text (doc :repl-out-text-area)
             (str "\n\n=== Switching to " project-path " REPL ===\n"))
    (let [repl (or (get @repls project-path)
                   (create-clojure-repl (doc :repl-out-writer) project-path))]
      (reset! (:repl doc) repl))))

(defn add-repl-input-handler [doc]
  (let [ta-in (doc :repl-in-text-area)
        get-caret-pos #(.getCaretPosition ta-in)
        ready #(let [caret-pos (get-caret-pos)
                     txt (.getText ta-in)
                     trim-txt (string/rtrim txt)]
                 (and
                   (pos? (.length trim-txt))
                   (<= (.length trim-txt)
                                caret-pos)
                   (= -1 (first (find-enclosing-brackets
                                  txt
                                  caret-pos)))))
        submit #(when-let [txt (.getText ta-in)]
                  (if (correct-expression? txt)
                    (do (send-to-repl doc txt nil)
                        (.setText ta-in ""))
                    (.setText (doc :arglist-label) "Malformed expression")))
        at-top #(zero? (get-line-of-offset ta-in (get-caret-pos)))
        at-bottom #(= (get-line-of-offset ta-in (get-caret-pos))
                      (get-line-of-offset ta-in (.. ta-in getText length)))
        prev-hist #(show-previous-repl-entry doc)
        next-hist #(show-next-repl-entry doc)]
    (attach-child-action-keys ta-in ["UP" at-top prev-hist]
                                    ["DOWN" at-bottom next-hist]
                                    ["ENTER" ready submit])
    (attach-action-keys ta-in ["cmd UP" prev-hist]
                              ["cmd DOWN" next-hist]
                              ["cmd ENTER" submit])))
