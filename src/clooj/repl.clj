; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com


(ns clooj.repl
  (:import (java.io PipedReader PipedWriter PrintWriter Writer)
           (java.awt Rectangle)
           (javax.swing SwingUtilities))
  (:use [clooj.utils :only (attach-child-action-keys attach-action-keys)]
        [clooj.brackets :only (find-nearby-root-form find-left-enclosing-bracket)]
        [clojure.pprint :only (pprint)])
  (:require [clojure.contrib.string :as string]))

(def repl-history {:items (atom nil) :pos (atom 0)})

;; REPL stuff
;; adapted from http://clojure101.blogspot.com/2009/05/creating-clojure-repl-in-your.html

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

(defn create-clojure-repl [result-writer]
  "This function creates an instance of clojure repl, with output going to output-writer
  Returns an input writer."
  (let [first-prompt (atom true)
        input-writer (PipedWriter.)
        piped-in (clojure.lang.LineNumberingPushbackReader. (PipedReader. input-writer))
        piped-out (PrintWriter. result-writer)
        repl-thread-fn #(binding [*printStackTrace-on-error* *printStackTrace-on-error*
                                  *in* piped-in
                                  *out* piped-out
                                  *err* *out*]
               (try
                 (clojure.main/repl
                   :init (fn [] (in-ns 'user))
                   :print (fn [& args] (doall (map repl-print args)))
                   :read (fn [prompt exit]
                           (read))
                   :caught (fn [e]
                             (when (is-eof-ex? e)
                               (throw e))
                             (if *printStackTrace-on-error*
                               (.printStackTrace e *out*)
                               (prn (clojure.main/repl-exception e)))
                             (flush))
                   :prompt (fn [] (printf "\n") (clojure.main/repl-prompt))
                   :need-prompt (constantly true))
                 (catch clojure.lang.LispReader$ReaderException ex
                   (prn (.getCause ex))
                   (prn "REPL closing"))
                 (catch java.lang.InterruptedException ex)
                 (catch java.nio.channels.ClosedByInterruptException ex)))]
    (.start (Thread. repl-thread-fn))
    input-writer))

(defn replace-first [coll x]
  (cons x (next coll)))

(defn update-repl-history [doc]
  (swap! (:items repl-history) replace-first
         (.getText (doc :repl-in-text-area))))

(defn send-to-repl [doc cmd]
  (SwingUtilities/invokeLater
    #(let [cmd-ln (str (.trim cmd) \newline)
           cmd (.trim cmd-ln)]
      (.append (doc :repl-out-text-area) cmd-ln)
      (.write (doc :repl-writer) cmd-ln)
      (.flush (doc :repl-writer))
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
              (let [[a b] (find-nearby-root-form ta)]
                (when (and a b)
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
          (.append buf char-array offset length))
        ([t]
          (when (= Integer (type t))
            (.append buf (char t)))))
      (flush [] (when ta-out
                  (SwingUtilities/invokeLater
                    #(do (.append ta-out (.toString buf))
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
                   (= -1 (find-left-enclosing-bracket
                           txt
                           caret-pos))))
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

(defn apply-namespace-to-repl [doc]
  (try
    (when-let [sexpr (read-string (. (doc :doc-text-area)  getText))]
      (when (= 'ns (first sexpr))
        (send-to-repl doc (str "(ns " (second sexpr) ")"))))
    (catch Exception e)))