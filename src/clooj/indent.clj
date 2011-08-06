(ns clooj.indent
  (:use [clooj.utils :only (attach-action-keys indent unindent
                            get-coords get-line-of-offset
                            get-line-start-offset
                            get-line-end-offset
                            awt-event
                            get-selected-lines)]
        [clooj.brackets :only (find-enclosing-brackets)]
        [clojure.contrib.string :only (ltrim)])
  (:import  (javax.swing.text DocumentFilter)))

(defn compute-indent-size [text-comp offset]
  (let [bracket-pos (first (find-enclosing-brackets
                             (.getText text-comp) offset))]
    (when (<= 0 bracket-pos)
      (let [bracket (.. text-comp getText (charAt bracket-pos))
            col (:col (get-coords text-comp bracket-pos))]
        (+ col
          (condp = bracket
            \( 2  \; 0  \\ 0  \[ 1  \{ 1  \" 1
            :else 1))))))
        
(defn fix-indent [text-comp line]
  (let [start (get-line-start-offset text-comp line)
        end (get-line-end-offset text-comp line)
        document (.getDocument text-comp)
        line-text (.getText document start (- end start))]
    (when-let [old-indent (re-find #"\ +" line-text)]
      (let [old-indent-size (.length old-indent)]
        (when-let [new-indent-size (compute-indent-size text-comp start)]       
          (let [delta (- new-indent-size old-indent-size)]
            (if (pos? delta)
              (.insertString document start (apply str (repeat delta " ")) nil)
              (.remove document start (- delta)))))))))

(defn fix-indent-selected-lines [text-comp]
  (awt-event 
    (dorun (map #(fix-indent text-comp %)
             (get-selected-lines text-comp)))))

(defn auto-indent-str [text-comp offset]
  (let [indent-size (or (compute-indent-size text-comp offset) 0)]
    (apply str "\n" (repeat indent-size " "))))
    
(defn setup-autoindent [text-comp]
  (attach-action-keys text-comp
    ["TAB" #(fix-indent-selected-lines text-comp)]
    ["cmd CLOSE_BRACKET" #(indent text-comp)]   ; "cmd ]"
    ["cmd OPEN_BRACKET" #(unindent text-comp)]) ; "cmd ["
  (.. text-comp getDocument
    (setDocumentFilter
      (proxy [DocumentFilter] []
        (replace [fb offset len text attrs]
          (.replace
            fb offset len  
            (condp = text
              "\n" (auto-indent-str text-comp offset) 
              text)
            attrs))
        (remove [fb offset len]
          (.remove fb offset len))
        (insertString [fb offset string attr]
          (.insertString fb offset string attr))))))
