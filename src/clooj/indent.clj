(ns clooj.indent
  (:use [clooj.utils :only (attach-action-keys indent unindent
                            get-coords get-line-of-offset
                            get-line-start-offset
                            get-line-end-offset
                            awt-event
                            get-selected-lines)]
        [clooj.brackets :only (find-enclosing-brackets)]
        [clojure.contrib.string :only (ltrim rtrim)])
  (:import  (javax.swing.text DocumentFilter)))

;(defn t [] (@clooj.core/current-app :doc-text-area))

(def special-tokens 
  ["def" "defn" "defmacro" "let" "for" "loop" "doseq" "if" "when"
   "binding" "case" "definline" "defmacro" "condp" "when-let" "if-let" "fn"
   "proxy" "reify" "when-first" "defmethod" "defmulti" "defn-" "defprotocol"
   "defrecord" "defstruct" "deftype" "dotimes" "doto" "extend" "extend-protocol"
   "extend-type" "if-not" "letfn" "ns" "update-proxy" "with-in-str"
   "with-local-vars" "with-out-str"
   "when-let" "when-not" "while" "with-bindings" "with-bindings*"])

(defn first-token [txt]
  (second (re-find #"\((.+?)\s" txt)))
          
(defn second-token-pos [txt]
  (when-let [x (re-find #".+?\s" (rtrim (first (.split #"\r?\n" txt))))]
    (.length x)))

(defn left-paren-indent-size [txt]
  (let [token1 (first-token txt)]
    (or
      (when (and token1
                 (not (or (some #{token1} special-tokens)
                          (.startsWith (ltrim token1) "["))))
        (second-token-pos txt))
      2)))

(defn compute-indent-size [text-comp offset]
  (let [bracket-pos (first (find-enclosing-brackets
                             (.getText text-comp) offset))]
    (when (<= 0 bracket-pos)
      (let [bracket (.. text-comp getText (charAt bracket-pos))
            col (:col (get-coords text-comp bracket-pos))]
        (+ col
           (condp = bracket
             \( (left-paren-indent-size (.getText text-comp
                                                  bracket-pos
                                                  (- offset bracket-pos)))
             \; 0  \\ 0  \[ 1  \{ 1  \" 1
             :else 1))))))

(defn fix-indent [text-comp line]
  (let [start (get-line-start-offset text-comp line)
        end (get-line-end-offset text-comp line)
        document (.getDocument text-comp)
        line-text (.getText document start (- end start))]
    (let [old-indent-size (count (re-find #"\A\ +" line-text))]
      (when-let [new-indent-size (compute-indent-size text-comp start)]       
        (let [delta (- new-indent-size old-indent-size)]
          (if (pos? delta)
            (.insertString document start (apply str (repeat delta " ")) nil)
            (.remove document start (- delta))))))))

(defn fix-indent-selected-lines [text-comp]
  (awt-event 
    (dorun (map #(fix-indent text-comp %)
                (get-selected-lines text-comp)))))

(defn auto-indent-str [text-comp offset]
  (let [indent-size (or (compute-indent-size text-comp offset) 0)]
    (apply str "\n" (repeat indent-size " "))))
   
(defn setup-autoindent [text-comp]
  (attach-action-keys text-comp
    ["TAB" #(do)]                  
    ["cmd1 BACK_SLASH" #(fix-indent-selected-lines text-comp)] ; "cmd1 \"
    ["cmd1 CLOSE_BRACKET" #(indent text-comp)]   ; "cmd1 ]"
    ["cmd1 OPEN_BRACKET" #(unindent text-comp)]) ; "cmd1 ["
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
