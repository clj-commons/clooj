; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.highlighting
  (:import (javax.swing.text DefaultHighlighter
                             DefaultHighlighter$DefaultHighlightPainter)
           (java.awt Color)
           (javax.swing.event CaretListener))
  (:use [clooj.core :only ()]
        [clooj.brackets :only (find-bad-brackets find-enclosing-brackets)]))


;; highlighting
 
(def caret-highlights (atom {}))

(defn highlight
  ([text-comp start stop color]
    (when (and (<= 0 start) (<= stop (.. text-comp getDocument getLength)))
      (.addHighlight (.getHighlighter text-comp)
                     start stop
                     (DefaultHighlighter$DefaultHighlightPainter. color))))
    ([text-comp pos color] (highlight text-comp pos (inc pos) color)))

(defn remove-highlight
  ([text-comp highlight-object]
    (when highlight-object
    (.removeHighlight (.getHighlighter text-comp)
                      highlight-object))))

(defn remove-highlights
  ([text-comp highlights]
    (dorun (map #(remove-highlight text-comp %) highlights))))

(defn highlight-enclosing-brackets [text-comp pos color]
  (let [txt (.getText text-comp)]
    (when txt
      (doall (map #(highlight text-comp % color)
               (find-enclosing-brackets txt pos))))))

(defn highlight-caret-enclosure [text-comp]
  (when-let [ch (get @caret-highlights text-comp)]
    (doall (map #(remove-highlight text-comp %) ch)))
  (swap! caret-highlights assoc text-comp
         (highlight-enclosing-brackets
           text-comp (.getCaretPosition text-comp) Color/LIGHT_GRAY)))

(defn highlight-bad-brackets [text-comp]
  (doall (map #(highlight text-comp % Color/PINK)
    (find-bad-brackets (.getText    text-comp)))))
