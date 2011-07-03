; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.highlighting
  (:import (javax.swing.text DefaultHighlighter
                             DefaultHighlighter$DefaultHighlightPainter)
           (java.awt Color)
           (javax.swing.event CaretListener))
  (:use [clooj.core :only ()]
        [clooj.utils :only (awt-event)]
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
    ;(println "remove-highlights:" highlights)
    (dorun (map #(remove-highlight text-comp %) highlights))))

(def caret-agent (agent nil))

(defn get-caret-data [text-comp]
   [(.getText text-comp) (.getCaretPosition text-comp)])

(defn highlight-caret-enclosure [text-comp]
  (send-off caret-agent
    (fn [old-pos]
      (let [pos (.getCaretPosition text-comp)]
        (when-not (= pos old-pos)
          (let [brackets (find-enclosing-brackets (.getText text-comp) pos)]
            (awt-event
              (remove-highlights text-comp (get @caret-highlights text-comp))
              (swap! caret-highlights assoc text-comp
              (doall (map #(highlight text-comp % Color/LIGHT_GRAY) brackets))))
            pos))))))

(defn highlight-bad-brackets [text-comp]
  (doall (map #(highlight text-comp % Color/PINK)
    (find-bad-brackets (.getText text-comp)))))