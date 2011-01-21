(ns clyde.core
  (:import (javax.swing JFrame JLabel JPanel JTextArea JScrollPane
  			JMenuBar JMenu JMenuItem KeyStroke JSplitPane SpringLayout)
           (javax.swing.event CaretListener)
  		   (java.awt.event ActionListener KeyEvent)
           (java.awt Font Toolkit FileDialog)
           (java.io File FilenameFilter FileReader FileWriter OutputStream))
  (:use [clojure.contrib.duck-streams :only (writer)])
  (:gen-class))

(defn get-mono-font []
  (Font. "Monaco" Font/PLAIN 11))

(defn make-text-area []
  (doto (JTextArea.)
    (.setFont (get-mono-font))))

(defn get-caret-position [doc]
  (let [t (:doc-text-area doc)
        offset (.getCaretPosition t)
        row (.getLineOfOffset t offset)
        col (- offset (.getLineStartOffset t row))]
    {:row row :col col}))

(defn display-caret-position [doc]
  (let [{:keys [row col]} (get-caret-position doc)]
    (.setText (:status-bar doc) (str " " (inc row) "|" (inc col)))))
  
(defn make-scroll-pane [text-area]
    (JScrollPane. text-area))

(defn make-split-pane []
  (JSplitPane. JSplitPane/VERTICAL_SPLIT true))

(defn create-doc []
  (let [doc-text-area (make-text-area)
        repl-text-area (make-text-area)
        split-pane (make-split-pane)
        status-bar (JLabel.)
        f (JFrame.)
        cp (.getContentPane f)
        layout (SpringLayout.)
        doc {:doc-text-area doc-text-area :repl-text-area repl-text-area
             :split-pane split-pane :status-bar status-bar :frame f
             :file (atom nil)}]
    (doto f
      (.setBounds 50 50 400 400)
      (.setLayout layout)
      (.add split-pane)
      (.add status-bar))
    (doto status-bar
      (.setFont (Font. "Courier" Font/PLAIN 13)))
    (doto layout
      (.putConstraint SpringLayout/NORTH split-pane 0 SpringLayout/NORTH cp)
      (.putConstraint SpringLayout/WEST split-pane 0 SpringLayout/WEST cp)
      (.putConstraint SpringLayout/EAST split-pane 0 SpringLayout/EAST cp)
      (.putConstraint SpringLayout/SOUTH split-pane -16 SpringLayout/SOUTH cp)
      (.putConstraint SpringLayout/NORTH status-bar -16 SpringLayout/SOUTH cp)
      (.putConstraint SpringLayout/SOUTH status-bar 0 SpringLayout/SOUTH cp)
      (.putConstraint SpringLayout/WEST status-bar 0  SpringLayout/WEST cp)
      (.putConstraint SpringLayout/EAST status-bar 0 SpringLayout/EAST cp)
      (.layoutContainer f))
    (doto doc-text-area
      (.addCaretListener
        (reify CaretListener
          (caretUpdate [this evt] (display-caret-position doc)))))
    (doto split-pane
      (.add (make-scroll-pane doc-text-area))
      (.add (make-scroll-pane repl-text-area))
      (.setResizeWeight 1.0))      
    doc))

(defn choose-file [frame suffix load]
  (let [dialog
    (doto (FileDialog. frame "Open clojure file"
            (if load FileDialog/LOAD FileDialog/SAVE))
      (.setFilenameFilter
        (reify FilenameFilter
          (accept [this _ name] (. name endsWith suffix))))
      .show)
    d (.getDirectory dialog)
    n (.getFile dialog)]
    (File. d n)))
    
(defn open-file [doc suffix]
  (let [frame (doc :frame)
        file (choose-file frame suffix true)]
    (.read (doc :doc-text-area) (FileReader. (.getAbsolutePath file)) nil)
    (.setTitle frame (.getPath file))
    (reset! (doc :file) file)))

(defn save-file [doc]
  (.write (doc :doc-text-area) (FileWriter. @(doc :file))))

(defn save-file-as [doc]
  (let [frame (doc :frame)
        file (choose-file frame ".clj" false)]
    (reset! (doc :file) file)
    (save-file doc)
    (.setTitle frame (.getPath file))))
  
(defn add-menu-item [menu item-name key-shortcut response-fn]
  (.add menu
    (doto (JMenuItem. item-name)
      (.setAccelerator (KeyStroke/getKeyStroke key-shortcut
        (. (Toolkit/getDefaultToolkit) getMenuShortcutKeyMask)))
      (.addActionListener
        (reify ActionListener
          (actionPerformed [this action-event]
            (do (response-fn))))))))

(defn make-menus [doc]
  (System/setProperty "apple.laf.useScreenMenuBar" "true")
  (let [menu-bar (JMenuBar.)
        file-menu (JMenu. "File")]
     (. (doc :frame) setJMenuBar menu-bar)
     (add-menu-item file-menu "Open" \O #(reset! (doc :file) (open-file doc ".clj")))
     (add-menu-item file-menu "Save" \S #(save-file doc))
     (add-menu-item file-menu "Save as..." \R #(save-file-as doc))
     (. menu-bar add file-menu)))

(defn startup []
  (let [doc (create-doc)]
     (def current-doc doc)
     (make-menus doc)
     (.show (doc :frame))))

(defn -main [& args]
  (startup))

