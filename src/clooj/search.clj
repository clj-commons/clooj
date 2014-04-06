; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.search
  (:import (java.awt Color)
           (java.util.regex Pattern Matcher))
  (:require [clooj.highlighting :as highlighting]
            [clooj.utils :as utils]))

(defn configure-search [match-case use-regex]
  (bit-or Pattern/CANON_EQ
          Pattern/UNICODE_CASE
          (if match-case 0 Pattern/CASE_INSENSITIVE)
          (if use-regex 0 Pattern/LITERAL)))

(defn find-all-in-string
  [s t match-case use-regex]
  (try
    (when (pos? (.length t))
      (let [p (Pattern/compile t (configure-search match-case use-regex))
            m (re-matcher p s)]
        (loop [positions []]
          (if (.find m)
            (recur (conj positions [(.start m) (.end m)] ) )
            positions))))
    (catch Exception _ [])))

(defn highlight-found [text-comp posns]
    (doall
      (map #(highlighting/highlight text-comp (first %) (second %) Color/YELLOW)
        posns)))

(defn next-item [cur-pos posns]
  (or (first (drop-while #(> cur-pos (first %)) posns)) (first posns)))

(defn prev-item [cur-pos posns]
  (or (first (drop-while #(< cur-pos (first %)) (reverse posns))) (last posns)))

(def search-highlights (atom nil))

(def current-pos (atom 0))

(defn update-find-highlight [sta app back]
  (let [dta (:doc-text-area app)
        match-case (.isSelected (:search-match-case-checkbox app))
        use-regex (.isSelected (:search-regex-checkbox app))
        posns (find-all-in-string (utils/get-text-str dta)
                                  (utils/get-text-str sta)
                                  match-case
                                  use-regex)]
    (highlighting/remove-highlights dta @search-highlights)
    (if (pos? (count posns))
      (let [selected-pos
             (if back (prev-item (dec @current-pos) posns)
                      (next-item @current-pos posns))
            posns (remove #(= selected-pos %) posns)
            pos-start (first selected-pos)
            pos-end (second selected-pos)]
        (.setBackground sta Color/WHITE)
        (doto dta
          (.setSelectionStart pos-end)
          (.setSelectionEnd pos-end))
        (reset! current-pos pos-start)
        (reset! search-highlights
                (conj (highlight-found dta posns)
                      (highlighting/highlight dta pos-start
                                              pos-end (.getSelectionColor dta))))
        (utils/scroll-to-pos dta pos-start))
      (.setBackground sta  Color/PINK))))

(defn start-find [app]
  (let [sta (:search-text-area app)
        case-checkbox (:search-match-case-checkbox app)
        regex-checkbox (:search-regex-checkbox app)
        close-button (:search-close-button app)
        arg (:arglist-label app)
        dta (:doc-text-area app)
        sel-text (.getSelectedText dta)]
    (.setVisible arg false)
    (doto sta
      (.setVisible true)
      (.requestFocus)
      (.selectAll))
    (.setVisible case-checkbox true)
    (.setVisible regex-checkbox true)
    (.setVisible close-button true)
    (if (not (empty? sel-text))
      (.setText sta sel-text))))

(defn stop-find [app]
  (let [sta (app :search-text-area)
        dta (app :doc-text-area)
        case-checkbox (:search-match-case-checkbox app)
        regex-checkbox (:search-regex-checkbox app)
        close-button (:search-close-button app)
        arg (app :arglist-label)]
    (.setVisible arg true)
    (.setVisible sta false)
    (.setVisible case-checkbox false)
    (.setVisible regex-checkbox false)
    (.setVisible close-button false)
    (highlighting/remove-highlights dta @search-highlights)
    (reset! search-highlights nil)
    (reset! current-pos 0)))

(defn escape-find [app]
  (stop-find app)
  (.requestFocus (:doc-text-area app)))

(defn highlight-step [app back]
  (let [doc-text-area (:doc-text-area app)
        search-text-area (:search-text-area app)]
    (start-find app)
    (when-not back
      (swap! current-pos inc))
    (update-find-highlight search-text-area app back)))

