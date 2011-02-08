; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.core
  (:import (javax.swing JFrame JLabel JPanel JTextArea JScrollPane JList
                        JMenuBar JMenu JMenuItem KeyStroke JSplitPane
                        SpringLayout AbstractListModel AbstractAction
                        UIManager)
           (javax.swing.event CaretListener DocumentListener UndoableEditListener)
           (javax.swing.text DefaultHighlighter
                             DefaultHighlighter$DefaultHighlightPainter
                             DocumentFilter)
           (javax.swing.undo UndoManager)
           (java.awt Insets)
           (java.awt.event ActionListener KeyEvent KeyListener)
           (java.awt Color Font Toolkit FileDialog)
           (java.io File FilenameFilter FileReader FileWriter OutputStream
                    PipedReader PipedWriter StringReader Writer)
           (clojure.lang LineNumberingPushbackReader))
  (:use [clojure.contrib.duck-streams :only (writer)]
        [clojure.pprint :only (pprint)]
        [clooj.repl :only (create-clojure-repl)])
  (:require [clojure.contrib.string :as string])
  (:gen-class))

;; utils

(defn count-while [pred coll]
  (count (take-while pred coll)))

(def menu-shortcut (. (Toolkit/getDefaultToolkit) getMenuShortcutKeyMask))

(defn cmd-key [key] (KeyStroke/getKeyStroke key menu-shortcut))

(defn get-mono-font []
  (Font. "Monaco" Font/PLAIN 11))

(defn make-text-area []
  (doto (JTextArea.)
    (.setFont (get-mono-font))))

(defn get-coords [text-comp offset]
  (let [row (.getLineOfOffset text-comp offset)
        col (- offset (.getLineStartOffset text-comp row))]
    {:row row :col col}))

;; REPL stuff

(defn make-repl-writer [ta-out]
  (proxy [Writer] []
    (write
      ([char-array offset length]
        (let [buf (StringBuffer.)]
          (.append buf char-array offset length)
            (.append ta-out (.toString buf))))
      ([t]
        (.append ta-out
          (if (= Integer (type t))
            (str (char t)) t))))
    (flush [] nil)
    (close [] nil)))

(defn add-repl-input-handler [ta-in ta-out repl-input-writer]
  (.addKeyListener ta-in
    (reify KeyListener
      (keyReleased [this _] nil)
      (keyTyped [this _] nil)
      (keyPressed [this e]        
        (when (and (= (.getKeyCode e) (KeyEvent/VK_ENTER))
                   (= (.. ta-in getDocument getLength)
                      (.getCaretPosition ta-in)))
          (let [cmd (str (.getText ta-in) \newline)]
            (.append ta-out cmd)
            (.write repl-input-writer cmd)
            (.setText ta-in "")))))))

;; caret finding

(defn get-caret-position [text-comp]
  (get-coords text-comp (.getCaretPosition text-comp)))

(defn display-caret-position [doc]
  (let [{:keys [row col]} (get-caret-position (:doc-text-area doc))]
    (.setText (:status-bar doc) (str " " (inc row) "|" (inc col)))))

;; bracket handling

(defn bracket-score [c]
  (condp = c 
         \(  1 \[  1 \{  1
         \) -1 \] -1 \} -1
         0))

(defn bracket-increment [score next-char]
  (+ score (bracket-score next-char)))

(defn count-brackets [s]
  (reductions bracket-increment 0 s))

(defn find-left-enclosing-bracket [text pos]
  (let [before (string/take pos text)]
    (- pos (count-while
             (partial >= 0)
             (count-brackets (string/reverse before))))))

(defn find-right-enclosing-bracket [text pos]
  (let [after (string/drop pos text)]
    (+ -1 pos (count-while
                (partial <= 0)
                (count-brackets after)))))

(defn find-enclosing-brackets [text pos]
  [(find-left-enclosing-bracket text pos)
   (find-right-enclosing-bracket text pos)])

(defn mismatched-brackets [a b]
  (and (or (nil? a) (some #{a} [\( \[ \{]))
       (some #{b} [\) \] \}])
       (not (some #{[a b]} [[\( \)] [\[ \]] [\{ \}]]))))

(defn find-bad-brackets [text]
  (loop [t text cnt 0 stack nil errs nil]
    (let [s stack
          c (first t)
          l (ffirst s)
          p (next s)
          j (conj s [c cnt])
          new-stack
            (condp = l
              \\ p
              \" (if (= c \") p s)
              \; (if (= c \newline) p s)
              (condp = c
                \" j \\ j \; j
                \( j \[ j \{ j
                \) p \] p \} p
                s))
          e (if (mismatched-brackets l c)
              (list (first s) [c cnt]))
          new-errs (if e (concat errs e) errs)]
        (if (next t)
          (recur (next t) (inc cnt) new-stack new-errs)
          (filter identity
                  (map second (concat new-stack errs)))))))

;; highlighting

(def caret-highlight (atom nil))

(defn highlight
  ([text-comp start stop color]
    (.addHighlight (.getHighlighter text-comp)
                   start stop
                   (DefaultHighlighter$DefaultHighlightPainter. color)))
  ([text-comp pos color] (highlight text-comp pos (inc pos) color)))

(defn remove-highlight
  ([text-comp highlight-object]
    (.removeHighlight (.getHighlighter text-comp)
                      highlight-object)))

(defn highlight-enclosing-brackets [text-comp pos color]
  (doall (map #(highlight text-comp % color)
       (find-enclosing-brackets (.getText text-comp) pos))))

(defn highlight-caret-enclosure [text-comp]
  (when-let [ch @caret-highlight]
    (doall (map #(remove-highlight text-comp %) ch)))
  (reset! caret-highlight
          (highlight-enclosing-brackets
            text-comp (.getCaretPosition text-comp) Color/LIGHT_GRAY)))

(defn highlight-bad-brackets [text-comp]
  (doall (map #(highlight text-comp % Color/PINK)
    (find-bad-brackets (.getText text-comp)))))

(defn add-caret-listener [text-comp f]
  (.addCaretListener text-comp
    (reify CaretListener (caretUpdate [this evt] (f)))))

(defn activate-caret-highlighter [text-comp]
  (add-caret-listener text-comp #(highlight-caret-enclosure text-comp)))

(defn activate-error-highlighter [text-comp]
  (let [hl #(do (.. text-comp getHighlighter removeAllHighlights)
                (highlight-bad-brackets text-comp))]
    (doto (.getDocument text-comp)
      (.addDocumentListener
        (reify DocumentListener
          (insertUpdate [this evt] (hl))
          (removeUpdate [this evt] (hl))
          (changedUpdate [this evt] (hl)))))))

;; paren closing (doesn't work)

(defn close-bracket [text-area] ;; doesn't work yet
  (let [text (.getText text-area)
        right-pos (.getCaretPosition text-area)
        left-pos (find-left-enclosing-bracket text right-pos)
        left-char (.charAt text left-pos)]
    (if-let [right-char (get {\( \), \{ \}, \[ \]} left-char)]
      (.insert text-area (str right-char) right-pos))))

;; build gui

(defn make-scroll-pane [text-area]
    (JScrollPane. text-area))

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

(defn constrain-to-parent [comp & args]
  (apply put-constraints comp
         (flatten (map #(cons (.getParent comp) %) (partition 2 args)))))

(defn add-line-numbers [text-comp max-lines]
  (let [row-height (.. text-comp getGraphics
                       (getFontMetrics (. text-comp getFont)) getHeight)
        sp (.. text-comp getParent getParent)
        jl (JList.
             (proxy [AbstractListModel] []
               (getSize [] max-lines)
               (getElementAt [i] (str (inc i) " "))))
        cr (. jl getCellRenderer)]
    (.setMargin text-comp (Insets. 0 10 0 0))
    (dorun (map #(.removeMouseListener jl %) (.getMouseListeners jl)))
    (dorun (map #(.removeMouseMotionListener jl %) (.getMouseMotionListeners jl)))
    (doto jl
      (.setBackground (Color. 235 235 235))
      (.setForeground (Color. 50 50 50))
      (.setFixedCellHeight row-height)
      (.setFont (Font. "Monaco" Font/PLAIN 8)))
    (doto cr
      (.setHorizontalAlignment JLabel/RIGHT)
      (.setVerticalAlignment JLabel/BOTTOM))
    (.setRowHeaderView sp jl)))

(defn make-undoable [text-area]
  (let [undoMgr (UndoManager.)]
    (.. text-area getDocument (addUndoableEditListener
        (reify UndoableEditListener
          (undoableEditHappened [this evt] (.addEdit undoMgr (.getEdit evt))))))
    (doto (. text-area getActionMap)
          (.put "Undo"
               (proxy [AbstractAction] ["Undo"]
                 (actionPerformed [evt]
                   (if (.canUndo undoMgr) (.undo undoMgr)))))
          (.put "Redo"
               (proxy [AbstractAction] ["Redo"]
                 (actionPerformed [evt]
                   (if (.canRedo undoMgr) (.redo undoMgr))))))
    (doto (. text-area getInputMap)
          (.put (cmd-key KeyEvent/VK_Z) "Undo")
          (.put (cmd-key KeyEvent/VK_Y) "Redo"))))

(defn auto-indent-str [text-comp offset]
  (let [bracket-pos (find-left-enclosing-bracket
                      (.getText text-comp) offset)]
    (if (pos? bracket-pos)
      (let [bracket (.. text-comp getText (charAt bracket-pos))
            col (:col (get-coords text-comp bracket-pos))
            indent-size (if (= bracket \() 2 1)] ;\) avoids highlighting problems
        (apply str "\n" (repeat (+ col indent-size) " ")))
      "\n")))
     
(defn set-tab-as-spaces [text-comp n]
  (let [tab-str (apply str (repeat n " "))]
    (.. text-comp getDocument
        (setDocumentFilter
          (proxy [DocumentFilter] []
            (replace [fb offset len text attrs]
              (.replace
                fb offset len
                (condp = text
                  "\t" tab-str
                  "\n" (auto-indent-str text-comp offset)
                  text)
                  attrs)))))))

(defn create-doc []
  (let [doc-text-area (make-text-area)
        repl-out-text-area (make-text-area)
        repl-in-text-area (make-text-area)
        split-pane (JSplitPane. JSplitPane/HORIZONTAL_SPLIT true)
        repl-split-pane (JSplitPane. JSplitPane/VERTICAL_SPLIT true)
        status-bar (JLabel.)
        f (JFrame.)
        cp (.getContentPane f)
        layout (SpringLayout.)
        repl-writer (create-clojure-repl (make-repl-writer repl-out-text-area))
        doc {:doc-text-area doc-text-area
             :repl-out-text-area repl-out-text-area
             :repl-in-text-area repl-in-text-area
             :split-pane split-pane :status-bar status-bar :frame f
             :file (atom nil) :repl-writer repl-writer}]
    (doto f
      (.setBounds 25 50 950 700)
      (.setLayout layout)
      (.add split-pane)
      (.add status-bar))
    (doto status-bar
      (.setFont (Font. "Courier" Font/PLAIN 13)))
    (constrain-to-parent split-pane :n 0 :w 0 :s -16 :e 0)
    (constrain-to-parent status-bar :s -16 :w 0 :s 0 :e 0)
    (.layoutContainer layout f)
    (doto doc-text-area
      (.addCaretListener
        (reify CaretListener
          (caretUpdate [this evt] (display-caret-position doc)))))
    (activate-caret-highlighter doc-text-area)
    (doto repl-out-text-area (.setLineWrap true))
    (doto split-pane
      (.add (make-scroll-pane doc-text-area))
      (.add repl-split-pane)
      (.setResizeWeight 0.5)
      (.setOneTouchExpandable true))
    (doto repl-split-pane
      (.add (make-scroll-pane repl-out-text-area))
      (.add (make-scroll-pane repl-in-text-area))
      (.setResizeWeight 0.75))
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
    (if (and d n)
      (File. d n))))

(defn restart-doc [doc ^File file]
  (let [frame (doc :frame)]
    (let [text-area (doc :doc-text-area)]
      (if file
        (do (.read text-area (FileReader. (.getAbsolutePath file)) nil)
            (.setTitle frame (.getPath file)))
        (do (.setText text-area "")
            (.setTitle frame "Untitled")))
      (make-undoable text-area)
      (set-tab-as-spaces text-area 2)
      (activate-error-highlighter text-area)
      (reset! (doc :file) file))))

(defn open-file [doc suffix]
  (let [frame (doc :frame)]
    (when-let [file (choose-file frame suffix true)]
      (restart-doc doc file))))

(defn new-file [doc]
  (restart-doc doc nil))

(declare save-file-as)

(defn save-file [doc]
  (if-not @(doc :file)
    (save-file-as doc)
    (.write (doc :doc-text-area) (FileWriter. @(doc :file)))))

(defn save-file-as [doc]
  (let [frame (doc :frame)
        file (choose-file frame ".clj" false)]
    (reset! (doc :file) file)
    (if @(doc :file)
     (save-file doc)
     (.setTitle frame (.getPath file)))))

(defn send-selected-to-repl [doc]
  (.write (doc :repl-writer)
          (.getSelectedText (doc :doc-text-area))))

;; menu setup

(defn add-menu-item [menu item-name key-shortcut response-fn]
  (let [k (+ KeyEvent/VK_A (- (int key-shortcut) (int \A)))]
    (.add menu
      (doto (JMenuItem. item-name)
        (.setAccelerator (cmd-key k))
        (.addActionListener
          (reify ActionListener
            (actionPerformed [this action-event]
              (do (response-fn)))))))))

(defn make-menus [doc]
  (System/setProperty "apple.laf.useScreenMenuBar" "true")
  (let [menu-bar (JMenuBar.)
        file-menu (JMenu. "File")
        tools-menu (JMenu. "Tools")]
    (. (doc :frame) setJMenuBar menu-bar)
    (add-menu-item file-menu "New" \N #(new-file doc))
    (add-menu-item file-menu "Open" \O #(open-file doc ".clj"))
    (add-menu-item file-menu "Save" \S #(save-file doc))
    (add-menu-item file-menu "Save as..." \R #(save-file-as doc))
    (add-menu-item tools-menu "Evaluate..." \E #(send-selected-to-repl doc))
    (. menu-bar add file-menu)
    (. menu-bar add tools-menu)))

;; startup
          
(defn startup []
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [doc (create-doc)]
     (def current-doc doc)
     (make-menus doc)
     (let [ta-in (doc :repl-in-text-area)
           ta-out (doc :repl-out-text-area)]
       (add-repl-input-handler ta-in ta-out
         (doc :repl-writer)))
     (.show (doc :frame))
     (add-line-numbers (doc :doc-text-area) Short/MAX_VALUE)))

(defn -main [& args]
  (startup))

;; testing

(defn get-text []
  (.getText (current-doc :doc-text-area)))
