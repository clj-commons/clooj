; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.search
  (:import (java.awt Color)
           (java.util.regex Pattern Matcher))
  (:require [clooj.highlighting :as highlighting]
            [clooj.utils :as utils]))

(def case-insensitive-search
  (reduce bit-or
          [Pattern/LITERAL
           Pattern/UNICODE_CASE
           Pattern/CASE_INSENSITIVE
           Pattern/CANON_EQ]))

(defn find-all-in-string
  [s t]
  (when (pos? (.length t))
    (let [p (Pattern/compile t case-insensitive-search)
          m (re-matcher p s)]
      (loop [positions []]
        (if (.find m)
          (recur (conj positions (.start m)))
          positions)))))

(defn highlight-found [text-comp posns length]
  (when (pos? length)
    (doall
      (map #(highlighting/highlight text-comp % (+ % length) Color/YELLOW)
        posns))))

(defn next-item [cur-pos posns]
  (or (first (drop-while #(> cur-pos %) posns)) (first posns)))

(defn prev-item [cur-pos posns]
  (or (first (drop-while #(< cur-pos %) (reverse posns))) (last posns)))

(def search-highlights (atom nil))

(defn update-find-highlight [sta app back]
  (let [dta (:doc-text-area app)
        length (.length (utils/get-text-str sta))
        posns (find-all-in-string (utils/get-text-str dta) (utils/get-text-str sta))]
    (highlighting/remove-highlights dta @search-highlights)
    (if (pos? (count posns))
      (let [selected-pos
             (if back (prev-item (dec (.getSelectionStart dta)) posns)
                      (next-item (.getSelectionStart dta) posns))
            posns (remove #(= selected-pos %) posns)]
        (.setBackground sta Color/WHITE)
        (when (pos? length)
          (reset! search-highlights
            (conj (highlight-found dta posns length)
                  (highlighting/highlight dta selected-pos
                             (+ selected-pos length) (.getSelectionColor dta))))
          (do (utils/scroll-to-pos dta selected-pos)
              (utils/set-selection dta selected-pos (+ selected-pos length)))))
      (do (.setSelectionEnd dta (.getSelectionStart dta))
          (.setBackground sta (if (pos? length) Color/PINK Color/WHITE))))))

(defn start-find [app]
  (let [sta (app :search-text-area)
        arg (app :arglist-label)
        dta (:doc-text-area app)
        sel-text (.getSelectedText dta)]
    (.setVisible arg false)
    (doto sta
      (.setVisible true)
      (.requestFocus)
      (.selectAll))
    (if (not (empty? sel-text))
      (.setText sta sel-text))))

(defn stop-find [app]
  (let [sta (app :search-text-area)
        dta (app :doc-text-area)
        arg (app :arglist-label)]
    (.setVisible arg true)
    (.setVisible sta false)
    (highlighting/remove-highlights dta @search-highlights)
    (reset! search-highlights nil)))

(defn escape-find [app]
  (stop-find app)
  (.requestFocus (:doc-text-area app)))

(defn highlight-step [app back]
  (let [doc-text-area (:doc-text-area app)
        search-text-area (:search-text-area app)]
    (start-find app)
    (if (not back)
      (.setSelectionStart doc-text-area (.getSelectionEnd doc-text-area)))
    (update-find-highlight search-text-area app back)))

