; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.navigate
  (:import (org.fife.ui.rsyntaxtextarea RSyntaxTextArea))
  (:require [clooj.utils :as utils]))

(defn get-caret-line-number [comp]
  (.getLineOfOffset comp (.getCaretPosition comp)))

(defn move-to-doc-start [comp]
  (.setCaretPosition comp 0))

(defn move-to-doc-end [comp]
  (.setCaretPosition comp
    (.. comp getDocument getLength)))

(defn move-to-line-start [comp]
  (.setCaretPosition comp
    (.getLineStartOffset comp
      (get-caret-line-number comp)))) 

(defn move-to-line-end [comp]
  (.setCaretPosition comp
    (let [p (.getLineEndOffset comp
              (get-caret-line-number comp))]
      (if (= p (.. comp getDocument getLength))
        p
        (dec p)))))
           
(defn attach-navigation-keys [comp]
  (utils/attach-action-keys comp
    ["cmd1 LEFT" #(move-to-line-start comp)]
    ["cmd1 RIGHT" #(move-to-line-end comp)]
    ["cmd1 UP" #(move-to-doc-start comp)]
    ["cmd1 DOWN" #(move-to-doc-end comp)]))