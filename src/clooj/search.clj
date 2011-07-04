; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.search
  (:import (java.awt Color))
  (:use [clooj.highlighting :only (highlight remove-highlights)]
        [clooj.utils :only (scroll-to-pos set-selection)]))

(defn find-all-in-string [s t]
  (when (pos? (.length t))
    (loop [positions [] p-start 0]
      (let [p (inc p-start)
            pnew (.indexOf s t p)]
        (if (pos? pnew)
          (recur (conj positions pnew) pnew)
          positions)))))

(defn highlight-found [text-comp posns length]
  (when (pos? length)
    (doall
      (map #(highlight text-comp % (+ % length) Color/YELLOW)
        posns))))

(defn next-item [cur-pos posns]
  (or (first (drop-while #(> cur-pos %) posns)) (first posns)))

(defn prev-item [cur-pos posns]
  (or (first (drop-while #(< cur-pos %) (reverse posns))) (last posns)))

(def search-highlights (atom nil))

(defn update-find-highlight [doc back]
  (let [sta (:search-text-area doc)
        dta (:doc-text-area doc)
        length (.. sta getText length)
        posns (find-all-in-string (.getText dta) (.getText sta))]
    (remove-highlights dta @search-highlights)
    (if (pos? (count posns))
      (let [selected-pos
             (if back (prev-item (dec (.getSelectionStart dta)) posns)
                      (next-item (.getSelectionStart dta) posns))
            posns (remove #(= selected-pos %) posns)]
        (.setBackground sta Color/WHITE)
        (when (pos? length)
          (reset! search-highlights
            (conj (highlight-found dta posns length)
                  (highlight dta selected-pos
                             (+ selected-pos length) (.getSelectionColor dta))))
          (do (scroll-to-pos dta selected-pos)
              (set-selection dta selected-pos (+ selected-pos length)))))
      (do (.setSelectionEnd dta (.getSelectionStart dta))
          (.setBackground sta (if (pos? length) Color/PINK Color/WHITE))))))

(defn start-find [doc]
  (let [sta (doc :search-text-area)]
    (doto sta
      (.setVisible true)
      (.requestFocus)
      (.selectAll))))

(defn stop-find [doc]
  (let [sta (doc :search-text-area)
        dta (doc :doc-text-area)]
    (.setVisible sta false)
    (remove-highlights dta @search-highlights)
    (reset! search-highlights nil)))

(defn escape-find [doc]
  (stop-find doc)
  (.requestFocus (:doc-text-area doc)))

(defn highlight-step [doc back]
  (let [dta (:doc-text-area doc)]
    (start-find doc)
    (if (not back)
        (.setSelectionStart dta (.getSelectionEnd dta)))
    (update-find-highlight doc back)))
