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

(def bad-bracket-highlights (atom {}))

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

(defn highlight-caret-enclosure [text-comp]
  (send-off caret-agent
    (fn [old-pos]
      (let [pos (.getCaretPosition text-comp)]
        (when-not (= pos old-pos)
          (let [enclosing-brackets (find-enclosing-brackets (.getText text-comp) pos)
                bad-brackets (find-bad-brackets (.getText text-comp))
                good-enclosures (clojure.set/difference
                                  (set enclosing-brackets) (set bad-brackets))]
            (awt-event
              (remove-highlights text-comp (get @caret-highlights text-comp))
              (swap! caret-highlights assoc text-comp
                (doall (map #(highlight text-comp % Color/LIGHT_GRAY) good-enclosures)))
              (remove-highlights text-comp (get @bad-bracket-highlights text-comp))
              (swap! bad-bracket-highlights assoc text-comp
                (doall (map #(highlight text-comp % Color/PINK) bad-brackets))))))
        pos))))

            