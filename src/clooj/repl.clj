; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.repl
  (:import (java.io File PipedReader PipedWriter PrintWriter Writer)
           (java.awt Rectangle)
           (java.net URL URLClassLoader))
  (:use [clooj.utils :only (attach-child-action-keys attach-action-keys
                            awt-event recording-source-reader)]
        [clooj.brackets :only (find-line-group find-enclosing-brackets)]
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
    
(defn create-clojure-repl
  "This function creates an instance of clojure repl, with output going to output-writer
  Returns an input writer."
  [result-writer project-path]
  (let [classloader (create-class-loader project-path)
        first-prompt (atom true)
        input-writer (PipedWriter.)
        piped-in (-> input-writer
                   PipedReader.
                   recording-source-reader)
        out (PrintWriter. result-writer true)
        repl-thread-fn #(binding [*printStackTrace-on-error* *printStackTrace-on-error*
                                  *in* piped-in
                                  *out* out
                                  *err* *out*]
               (try
                 (clojure.main/repl
                   :init (fn [] (in-ns 'user))
                   :print (fn [& args]
                            (dorun (map repl-print args)))
                   :read (fn [prompt exit]
                           (let [form (read)]
                             (if (= form 'EXIT-REPL)
                               exit
                               (with-meta form
                                 {:clooj/src (.toString piped-in)}))))
                   :caught (fn [e]
                             (when (is-eof-ex? e)
                               (throw e))
                             (if *printStackTrace-on-error*
                               (.printStackTrace e *out*)
                               (prn (clojure.main/repl-exception e))))
                   :prompt (fn []
                             (println)
                             (clojure.main/repl-prompt)
                             (println))
                   :need-prompt (constantly true)
                   :eval (fn [form]
                           (let [val (eval form)]
                             (when (var? val)
                               (alter-meta! val merge (meta form)))
                             val))
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
                :input-writer input-writer
                :project-path project-path}]
      (swap! repls assoc project-path repl)
      repl)))

(defn replace-first [coll x]
  (cons x (next coll)))

(defn update-repl-history [doc]
  (swap! (:items repl-history) replace-first
         (.getText (doc :repl-in-text-area))))

(defn send-to-repl [doc cmd]
  (awt-event
    (let [cmd-ln (str (.trim cmd) \newline)
           cmd (.trim cmd-ln)]
      (.append (doc :repl-out-text-area) cmd-ln)
      (.write (:input-writer @(doc :repl)) cmd-ln)
      (.flush (:input-writer @(doc :repl)))
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
        txt (or
              (.getSelectedText ta)
              (let [[a b] (find-line-group ta)]
                (when (and a b (< a b))
                  (.. ta getDocument
                    (getText a (- b a))))))]
      (when txt (send-to-repl doc txt))))

(defn send-doc-to-repl [doc]
  (->> doc :doc-text-area .getText (send-to-repl doc)))

(defn make-repl-writer [ta-out]
  (let [buf (StringBuffer.)]
    (proxy [Writer] []
      (write
        ([char-array offset length]
          (awt-event (.append buf char-array offset length)))
        ([t]
          (when (= Integer (type t))
            (awt-event (.append buf (char t))))))
      (flush []
        (when ta-out
          (awt-event
            (do (.append ta-out (.toString buf))
                (scroll-to-last ta-out)
                (.setLength buf 0)))))
      (close [] nil))))

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
  (swap! (:pos repl-history)
         #(Math/max 0 (dec %)))
  (update-repl-in doc))

(defn apply-namespace-to-repl [doc]
  (try
    (when-let [sexpr (read-string (. (doc :doc-text-area) getText))]
      (when (= 'ns (first sexpr))
        (send-to-repl doc (str "(ns " (second sexpr) ")"))))
    (catch Exception e)))

(defn restart-repl [doc project-path]
  (.append (doc :repl-out-text-area)
           (str "\n=== RESTARTING " project-path " REPL ===\n"))
  (let [input (-> doc :repl deref :input-writer)]
    (.write input "EXIT-REPL\n")
    (.flush input))
  (let [thread (-> doc :repl deref :thread)]
    (doseq [_ (range 50) :while (.isAlive thread)]
      (Thread/sleep 100))
    (.stop thread))
  (reset! (:repl doc) (create-clojure-repl (doc :repl-out-writer) project-path))
  (apply-namespace-to-repl doc))

(defn switch-repl [doc project-path]
  (when (not= project-path (-> doc :repl deref :project-path))
    (.append (doc :repl-out-text-area)
             (str "\n=== Switching to " project-path " REPL ===\n"))
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
        submit #(do (send-to-repl doc (.getText ta-in))
                    (.setText ta-in ""))
        at-top #(zero? (.getLineOfOffset ta-in (get-caret-pos)))
        at-bottom #(= (.getLineOfOffset ta-in (get-caret-pos))
                      (.getLineOfOffset ta-in (.. ta-in getText length)))
        prev-hist #(show-previous-repl-entry doc)
        next-hist #(show-next-repl-entry doc)]
    (attach-child-action-keys ta-in ["UP" at-top prev-hist]
                                    ["DOWN" at-bottom next-hist]
                                    ["ENTER" ready submit])
    (attach-action-keys ta-in ["cmd UP" prev-hist]
                              ["cmd DOWN" next-hist]
                              ["cmd ENTER" submit])))
