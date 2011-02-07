(ns clooj.repl
  (:import [java.io CharArrayWriter PipedReader PipedWriter
            PrintWriter])
  (:require [clojure.main :only repl]))

;; borrowed from http://clojure101.blogspot.com/2009/05/creating-clojure-repl-in-your.html

(def *printStackTrace-on-error* false)

(defn is-eof-ex? [throwable]
(and (instance? clojure.lang.LispReader$ReaderException throwable)
(or
(.startsWith (.getMessage throwable) "java.lang.Exception: EOF while reading")
(.startsWith (.getMessage throwable) "java.io.IOException: Write end dead"))))

(defn create-clojure-repl [result-writer]
  "This function creates an instance of clojure repl, with output going to output-writer
  Returns an input writer."
  (let [input-writer (PipedWriter.)
        piped-in (clojure.lang.LineNumberingPushbackReader. (PipedReader. input-writer))
        piped-out (PrintWriter. result-writer)
        repl-thread-fn #(binding [*printStackTrace-on-error* *printStackTrace-on-error*
                                  *in* piped-in
                                  *out* piped-out
                                  *err* *out*]
               (try
                 (clojure.main/repl
                   :init (fn [] (in-ns 'user))
                   :read (fn [prompt exit]
                           (read))
                   :caught (fn [e]
                             (when (is-eof-ex? e)
                               (throw e))
                             (if *printStackTrace-on-error*
                               (.printStackTrace e *out*)
                               (prn (clojure.main/repl-exception e)))
                             (flush))
                   :need-prompt (constantly true))
                 (catch clojure.lang.LispReader$ReaderException ex
                   (prn "REPL closing"))
                 (catch java.lang.InterruptedException ex)
                 (catch java.nio.channels.ClosedByInterruptException ex)))]
    (.start (Thread. repl-thread-fn))
    input-writer))
