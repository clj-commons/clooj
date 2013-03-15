(ns clooj.protocols)

;; Repl protocol

(defprotocol Repl
  (evaluate [this code] "Evaluate code (a string).")
  (close [this] "Stop the repl instance."))
