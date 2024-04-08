; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.repl.lein
  (:import (java.io BufferedReader File InputStreamReader))
  (:require [nrepl.core :as nrepl]
            [clojure.java.io :as io]
            [clooj.protocols :as protocols]
            [clooj.utils :as utils]))

; Documentation for nrepl at https://github.com/clojure/tools.nrepl

;; nrepl handling

(defn connect-nrepl
  "Connect to an nrepl port. Return a user session and
   a control session."
  [port out-writer]
  (let [conn (nrepl/connect :port port)
        client (nrepl/client conn 1000)]
    {:port port
     :connection conn
     :client client
     :session (nrepl/new-session client)
     :out out-writer}))

(defn disconnect-nrepl
  "Disconnects from an nrepl port."
  [{:keys [connection]}]
  (.close connection))

(defn nrepl-eval
  "Evaluate nrepl code."
  [nrepl-connection code]
  (let [results (nrepl/message
                  (:client nrepl-connection)
                  {:op :eval :code (str "(do " code ")")
                   :session :session})
        promised-value (promise)]
    (println results)
    (future (doseq [result results]
              (when-let [out (:out result)]
                (binding [*out* (:out nrepl-connection)]
                  (locking *out* (print out))))
              (when-let [value (:value result)]
                (deliver promised-value (read-string value)))))
    @promised-value))

(defn nrepl
  "Connects to an nrepl, returning a Repl instance."
  [port out-writer]
  (let [nrepl (connect-nrepl port out-writer)]
    (reify protocols/Repl
      (evaluate [_ code]
        (nrepl-eval nrepl code))
      (close [_]
        (disconnect-nrepl nrepl)))))

;; lein repl

(defn lein-command
  "Issue a leiningen command in project-path."
  [project-path cmd]
  (->
    (doto (ProcessBuilder. ["lein" cmd])
      (.redirectErrorStream false)
      (.directory (io/file (or project-path "."))))
    .start))

(defn lein-repl-process
  "Start an external lein repl process."
  [project-path]
  (lein-command project-path "repl"))

(defn lein-nrepl-port-number
  "Takes the first line printed to stdout from a lein repl process
   and returns the nrepl port number."
  [out-line]
  (when-let [port-str (second (re-find #"port\s(\d+)" out-line))]
    (Long/parseLong port-str)))

(defn lein-repl-start
  "Start an external lein repl process, and connect
   to it via nrepl."
  [project-path out-writer]
  (let [process (lein-repl-process project-path)
        lines (line-seq (utils/process-reader process))
        port (lein-nrepl-port-number (first (drop-while nil? lines)))]
    {:nrepl (nrepl port out-writer)
     :process process}))
    
(defn lein-repl-stop
  "Disconnect from the nrepl connection and destroy the lein repl process."
  [{:keys [process nrepl]}]
  (.close nrepl)
  (.destroy process))

(defn lein-repl
  "Creates and connect to a lein repl,
   returning a Repl instance. The repl's output
   is printed to out-writer."
  [project-path out-writer]
  (println "lein-repl.")
  (let [repl (lein-repl-start project-path out-writer)]
    (reify protocols/Repl
      (evaluate [_ code]
        (.evaluate (:nrepl repl) code))
      (close [_]
        (lein-repl-stop repl)))))

(defn lein-classpath-items
  "Returns a string containing the lein classpath."
  [project-path]
  (-> (lein-command project-path "classpath")
      utils/process-reader
      line-seq
      first
      (.split File/pathSeparator)))

  