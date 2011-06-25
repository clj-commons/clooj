; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.utils
  (:import (java.util UUID)
           (java.awt FileDialog Point)
           (java.awt.event ActionListener)
           (java.util.prefs Preferences)
           (java.io ByteArrayInputStream ByteArrayOutputStream
                    File FilenameFilter
                    ObjectInputStream ObjectOutputStream)
           (javax.swing AbstractAction JFileChooser JMenu JMenuBar 
                        JMenuItem KeyStroke SpringLayout SwingUtilities)
           (javax.swing.event CaretListener DocumentListener UndoableEditListener)
           (javax.swing.undo UndoManager))
  (:require [clojure.contrib.string :as string]
            [clooj.brackets :only (find-bad-brackets find-enclosing-brackets)]))


;; utils

(defmacro do-when [f & args]
  (let [args_ args]
    `(when (and ~@args_)
      (~f ~@args_))))

(defn count-while [pred coll]
  (count (take-while pred coll)))

(defn remove-nth [s n]
  (lazy-cat (take n s) (drop (inc n) s)))

;; preferences
  
;; define a UUID for clooj preferences
(def clooj-prefs (.. Preferences userRoot
      (node "clooj") (node "c6833c87-9631-44af-af83-f417028ea7aa")))

(defn partition-str [n s]
  (loop [rem s acc []]
    (if (pos? (.length rem))
      (recur (clojure.contrib.string/drop n rem)
             (conj acc (clojure.contrib.string/take n rem)))
      (seq acc))))

(def pref-max-bytes (* 3/4 Preferences/MAX_VALUE_LENGTH))

(defn write-value-to-prefs
  "Writes a pure clojure data structure to Preferences object."
  [prefs key value]
  (let [chunks (partition-str pref-max-bytes (with-out-str (pr value)))
        node (. prefs node key)]
    (.clear node)
    (doseq [i (range (count chunks))]
       (. node put (str i) (nth chunks i)))))

(defn read-value-from-prefs
  "Reads a pure clojure data structure from Preferences object."
  [prefs key]
  (let [node (. prefs node key)]
    (let [s (apply str
              (for [i (range (count (. node keys)))]
                (.get node (str i) nil)))]
      (when (and s (pos? (.length s))) (read-string s)))))

(defn write-obj-to-prefs
  "Writes a java object to a Preferences object."
  [prefs key obj]
  (let [bos (ByteArrayOutputStream.)
        os (ObjectOutputStream. bos)
        node (. prefs node key)]
    (.writeObject os obj)
    (. node putByteArray "0" (.toByteArray bos))))

(defn read-obj-from-prefs
  "Reads a java object from a Preferences object."
  [prefs key]
  (let [node (. prefs node key)
        bis (ByteArrayInputStream. (. node getByteArray "0" nil))
        os (ObjectInputStream. bis)]
      (.readObject os)))

;; identify OS

(defn get-os []
  (.. System (getProperty "os.name") toLowerCase))

(def is-win
  (memoize #(not (neg? (.indexOf (get-os) "win")))))

(def is-mac
  (memoize #(not (neg? (.indexOf (get-os) "mac")))))

(def is-unix
  (memoize #(not (and (neg? (.indexOf (get-os) "nix"))
                     (neg? (.indexOf (get-os) "nux"))))))


;; swing layout

(defn put-constraint [comp1 edge1 comp2 edge2 dist]
  (let [edges {:n SpringLayout/NORTH
               :w SpringLayout/WEST
               :s SpringLayout/SOUTH
               :e SpringLayout/EAST}]
  (.. comp1 getParent getLayout
            (putConstraint (edges edge1) comp1 
                           dist (edges edge2) comp2))))

(defn put-constraints [comp & args]
  (let [args (partition 3 args)
        edges [:n :w :s :e]]
    (dorun (map #(apply put-constraint comp %1 %2) edges args))))

(defn constrain-to-parent
  "Distance from edges of parent comp args"
  [comp & args]
  (apply put-constraints comp
         (flatten (map #(cons (.getParent comp) %) (partition 2 args)))))

;; text components

(defn get-coords [text-comp offset]
  (let [row (.getLineOfOffset text-comp offset)
        col (- offset (.getLineStartOffset text-comp row))]
    {:row row :col col}))

(defn get-caret-coords [text-comp]
  (get-coords text-comp (.getCaretPosition text-comp)))

(defn add-text-change-listener [text-comp f]
  "Executes f whenever text is changed in text component."
  (.addDocumentListener
    (.getDocument text-comp)
    (reify DocumentListener
      (insertUpdate [this _] (f))
      (removeUpdate [this _] (f))
      (changedUpdate [this _] (f)))))

(defn add-caret-listener [text-comp f]
  (.addCaretListener text-comp
    (reify CaretListener (caretUpdate [this evt] (f)))))

(defn set-selection [text-comp start end]
  (doto text-comp (.setSelectionStart start) (.setSelectionEnd end)))

(defn scroll-to-pos [text-area offset]
  (let [r (.modelToView text-area offset)
        v (.getParent text-area)
        l (.. v getViewSize height)
        h (.. v getViewRect height)]
    (.setViewPosition v
       (Point. 0 (min (- l h) (max 0 (- (.y r) (/ h 2))))))))

(defn replace-in-selected-row-headers [text-comp txt-old txt-new]
  (SwingUtilities/invokeLater
    #(let [row1 (.getLineOfOffset text-comp (.getSelectionStart text-comp))
           row2 (.getLineOfOffset text-comp (.getSelectionEnd text-comp))
           start (dec (.getLineStartOffset text-comp row1))
           end (dec (.getLineEndOffset text-comp row2))
           s-old (.. text-comp getDocument (getText start (- end start)))
           s-new (.replace s-old (str "\n" txt-old) (str "\n" txt-new))]
      (.replaceRange text-comp s-new start end)
      (set-selection text-comp
        (.getLineStartOffset text-comp row1)
        (dec (.getLineEndOffset text-comp row2))))))

(defn comment-out [text-comp]
  (replace-in-selected-row-headers text-comp "" ";"))

(defn uncomment-out [text-comp]
  (replace-in-selected-row-headers text-comp ";" ""))

(defn indent [text-comp]
  (replace-in-selected-row-headers text-comp "" "  "))

(defn unindent [text-comp]
  (replace-in-selected-row-headers text-comp "  " ""))
                     

;; keys

(defn get-keystroke [key-shortcut]
  (KeyStroke/getKeyStroke
    (.replace key-shortcut "cmd"
      (if (is-mac) "meta" "ctrl"))))

;; actions

(defn attach-child-action-key
  "Maps an input-key on a swing component to an action,
  such that action-fn is executed when pred function is
  true, but the parent (default) action when pred returns
  false."
  [component input-key pred action-fn]
  (let [im (.getInputMap component)
        am (.getActionMap component)
        input-event (get-keystroke input-key)
        parent-action (if-let [tag (.get im input-event)]
                        (.get am tag))
        child-action
          (proxy [AbstractAction] []
            (actionPerformed [e]
              (if (pred)
                (action-fn)
                (when parent-action
                  (.actionPerformed parent-action e)))))
        uuid (.. UUID randomUUID toString)]
    (.put im input-event uuid)
    (.put am uuid child-action)))


(defn attach-child-action-keys [comp & items]
  (doall (map #(apply attach-child-action-key comp %) items)))

(defn attach-action-key
  "Maps an input-key on a swing component to an action-fn."
  [component input-key action-fn]
  (attach-child-action-key component input-key
                           (constantly true) action-fn))

(defn attach-action-keys [comp & items]
  "Maps input keys to action-fns."
  (doall (map #(apply attach-action-key comp %) items)))
  
;; menus

(defn add-menu-item [menu item-name key-shortcut response-fn]
  (let [menu-item (JMenuItem. item-name)]  
    (when key-shortcut
      (.setAccelerator menu-item (get-keystroke key-shortcut)))
    (.addActionListener menu-item
      (reify ActionListener
        (actionPerformed [this action-event]
          (response-fn))))
    (.add menu menu-item)))

(defn add-menu [^JMenuBar menu-bar title & item-triples]
  "Each item-triple is a vector containing a
  menu item's text, shortcut key, and the function
  it executes."
  (let [menu (JMenu. title)]
    (doall (map #(apply add-menu-item menu %) item-triples))
    (.add menu-bar menu)))

;; undoability

(defn make-undoable [text-area]
  (let [undoMgr (UndoManager.)]
    (.. text-area getDocument (addUndoableEditListener
        (reify UndoableEditListener
          (undoableEditHappened [this evt] (.addEdit undoMgr (.getEdit evt))))))
    (attach-action-keys text-area
      ["cmd Z" #(if (.canUndo undoMgr) (.undo undoMgr))]
      ["cmd shift Z" #(if (.canRedo undoMgr) (.redo undoMgr))])))


;; file handling

(defn choose-file [parent title suffix load]
  (let [dialog
    (doto (FileDialog. parent title
            (if load FileDialog/LOAD FileDialog/SAVE))
      (.setFilenameFilter
        (reify FilenameFilter
          (accept [this _ name] (. name endsWith suffix))))
      (.setVisible true))
    d (.getDirectory dialog)
    n (.getFile dialog)]
    (if (and d n)
      (File. d n))))

(defn choose-directory [parent title]
  (if (is-mac)
    (let [dirs-on #(System/setProperty
                     "apple.awt.fileDialogForDirectories" (str %))]
      (dirs-on true)
        (let [dir (choose-file parent title "" true)]
          (dirs-on false)
          dir))
    (let [fc (JFileChooser.)]
      (doto fc (.setFileSelectionMode JFileChooser/DIRECTORIES_ONLY)
               (.setDialogTitle title))
       (if (= JFileChooser/APPROVE_OPTION (.showOpenDialog fc parent))
         (.getSelectedFile fc)))))

;; listener

(comment (defmacro add-listener [comp type f]
  `(let [methods (.getMethods type)] )))
