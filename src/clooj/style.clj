; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.style
  (:import 
    (java.io FileInputStream)
    (javax.swing JComboBox JFrame JList JScrollPane JSplitPane
                 ListSelectionModel SpringLayout JOptionPane)
    (javax.swing.text SimpleAttributeSet StyleConstants)
    (java.awt Color Font FontMetrics GraphicsEnvironment)
    (java.awt.image BufferedImage)
    (javax.swing.event ListSelectionListener)
    (org.fife.ui.rsyntaxtextarea Theme)
    (java.util Vector))
  (:use [clooj.utils :only (constrain-to-parent make-split-pane choose-file 
                            awt-event write-value-to-prefs clooj-prefs
                            read-value-from-prefs)]))

(def graphics-object
  (memoize (fn [] (.createGraphics
                    (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB)))))

(defn set-text-color [text-comp offset length color]
  (let [style-doc (.getStyledDocument text-comp)
        attrs (SimpleAttributeSet.)]
    (StyleConstants/setForeground attrs color)
    (.setCharacterAttributes style-doc offset length attrs true)))

(defn get-tokens [txt]
  (re-seq #"[:|a-z|A-Z|/|\\|\.|-|0-9|\+\"]+" txt)) ;"

;; fonts

(def monospaced?
  (fn [font]
    (let [g (graphics-object)
          m (.getFontMetrics g font)]
      (apply == (map #(.charWidth m %) [\m \n \. \M \-])))))

(defn get-all-fonts-12 []
  (let [font-names (.. GraphicsEnvironment
                       getLocalGraphicsEnvironment
                       getAvailableFontFamilyNames)]
    (map #(Font. % Font/PLAIN 12) font-names)))

(defn get-monospaced-fonts []
  (map #(.getName %) (filter monospaced? (get-all-fonts-12))))

(defn simple-list [choice-fun data init-val]
  (let [list (JList. (Vector. data))]
    (doto list
      (.setSelectedValue init-val true)
      (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
      (.addListSelectionListener
        (reify ListSelectionListener
          (valueChanged [_ e]
            (when-not (.getValueIsAdjusting e))
              (choice-fun
                (.. list getModel (getElementAt (.getSelectedIndex list))))))))
    (JScrollPane. list)))

(def font-window (atom nil))

(defn create-font-window [app set-font init-name init-size]
  (let [name (atom init-name)
        size (atom init-size)
        bounds (.getBounds (:frame app))
        x (+ (.x bounds) (/ (.width bounds) 2))
        y (+ (.y bounds) (/ (.height bounds) 2))
        layout (SpringLayout.)
        font-list (simple-list #(set-font app (reset! name %) @size)
                               (get-monospaced-fonts) init-name)
        size-list (simple-list #(set-font app @name (reset! size%))
                               (concat (range 5 49)) init-size)
        split (make-split-pane font-list size-list true 5 0.5)
        frame (doto (JFrame. "Choose Font")
                (.setBounds (- x 250) (- y 250) 500 500)
                (.setLayout layout)
                (.add split)
              ;  (.setMenuBar (-> app :frame .getMenuBar))
              )]
      (doto split
        (.setLeftComponent font-list)
        (.setRightComponent size-list))
      (constrain-to-parent split :n 5 :w 5 :s -5 :e -5)
      (.show font-list)
      (.show size-list)
    frame))

(defn show-font-window [app set-font init-name init-size]
  (when-not @font-window
    (reset! font-window (create-font-window app set-font init-name init-size)))
  (.show @font-window))

;; theme (optional)
(defonce current-theme (atom nil))

(defn set-theme [app apply-to]
    (if @current-theme (.apply @current-theme (get app apply-to))))

(defn refresh-theme [app]
    (do
        (set-theme app :doc-text-area)
        (set-theme app :repl-out-text-area)
        (set-theme app :repl-in-text-area)
        (set-theme app :help-text-area)))

(defn swap-theme [input-stream]
    (swap! current-theme
           (fn [old-value new-value]
               (let [new-theme (Theme/load (FileInputStream. new-value))]
                   (write-value-to-prefs clooj-prefs "app-theme" new-value)
                   new-theme))
           input-stream))

(defn load-theme [app]
    (try
        (let [theme-file (read-value-from-prefs clooj-prefs "app-theme")]
            (swap-theme theme-file)
            (refresh-theme app))
        (catch Exception e (println "pref not set, this is ok"))))

(defn show-theme-window [app]
    (try
        (when-let [dir (choose-file (app :frame) "Select a theme file" "" true)]
            (awt-event
                 (let [path (.getAbsolutePath dir)]
                     (swap-theme path)
                     (refresh-theme app))))
        (catch Exception e (do (JOptionPane/showMessageDialog nil
                               "Invalid theme file."
                               "Oops" JOptionPane/ERROR_MESSAGE)
                           (.printStackTrace e)))))
