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

(defn create-clojure-repl []
"This function creates an instance of clojure repl using piped in and out.
It returns a map of two functions repl-fn and result-fn - first function
can be called with a valid clojure expression and the results are read using
the result-fn."
(let [cmd-wtr (PipedWriter.)
        result-rdr (PipedReader.)
        piped-in (clojure.lang.LineNumberingPushbackReader. (PipedReader. cmd-wtr))
        piped-out (PrintWriter. (PipedWriter. result-rdr))
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
{:repl-fn (fn [cmd]
     (if (= cmd ":CLOSE-REPL")
       (do
         (.close cmd-wtr)
         (.close result-rdr))
       (do
         (.write cmd-wtr cmd)
         (.flush cmd-wtr))))
;//??Using CharArrayWriter to build the string from each read of one byte
;Once there is nothing to read than this function returns the string read.
;Using partial so that CharArrayWriter is only created and once and reused.
;There could be better way.
:result-fn (partial
       (fn [wtr]
         (.write wtr (.read result-rdr))
         (if (.ready result-rdr)
           (recur wtr)
           (let [result (.toString wtr)]
             (.reset wtr)
             result)))
       (CharArrayWriter.))}))
