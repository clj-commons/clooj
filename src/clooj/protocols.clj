; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.protocols)

;; Repl protocol

(defprotocol Repl
  (evaluate [this code] "Evaluate code (a string).")
  (close [this] "Stop the repl instance."))
