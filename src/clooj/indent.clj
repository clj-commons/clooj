(ns clooj.indent
  (:use [clooj.utils :only (attach-action-keys indent unindent
                            get-coords get-line-of-offset
                            get-line-start-offset
                            get-line-end-offset)]
        [clooj.brackets :only (find-enclosing-brackets)])
  (:import  (javax.swing.text DocumentFilter)))


(defn auto-indent-str [text-comp offset]
  (let [bracket-pos (first (find-enclosing-brackets
                             (.getText text-comp) offset))]
    (if (<= 0 bracket-pos)
      (let [bracket (.. text-comp getText (charAt bracket-pos))
            col (:col (get-coords text-comp bracket-pos))
            indent-size (if (= bracket \() 2 1)] ;\) avoids highlighting problems
        (apply str "\n" (repeat (+ col indent-size) " ")))
      "\n")))

(defn setup-autoindent [text-comp]
  (attach-action-keys text-comp
    ["TAB" #(indent text-comp)]
    ["shift TAB" #(unindent text-comp)])
  (.. text-comp getDocument
      (setDocumentFilter
        (proxy [DocumentFilter] []
          (replace [fb offset len text attrs]
            (println fb offset len text attrs)
            (.replace
              fb offset len
              (condp = text
                "\n" (auto-indent-str text-comp offset)
                text)
              attrs))))))

(defn find-affected-lines [text-comp offset]
  (let [first-row (get-line-of-offset text-comp offset)
        first-row-end (get-line-end-offset text-comp first-row)
        first-row-start (get-line-start-offset text-comp first-row)
        x (do (println first-row first-row-end first-row-start))
        last-bracket
          (loop [o first-row-end prev-r nil]
            (let [[l r] (find-enclosing-brackets
                          (.getText text-comp) o)]
              (println l r (get-line-of-offset text-comp l))
              (if (> l offset)
                (recur l r)
                prev-r)))]
    (when last-bracket
      (println last-bracket)
      (range (inc first-row)
             (inc (get-line-of-offset text-comp last-bracket))))))
  