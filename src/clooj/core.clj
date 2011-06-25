; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.core
  (:import (javax.swing AbstractListModel BorderFactory
                        JFrame JLabel JList JMenuBar
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree SpringLayout SwingUtilities
                        UIManager)
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener)
           (javax.swing.text DocumentFilter)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (java.awt Insets Point Rectangle)
           (java.awt.event FocusAdapter)
           (java.awt Color Font)
           (java.io File FileReader FileWriter))
  (:use [clojure.contrib.duck-streams :only (writer)]
        [clojure.pprint :only (pprint)]
        [clooj.brackets]
        [clooj.highlighting]
        [clooj.repl]
        [clooj.search]
        [clooj.tree :only (add-project-to-tree load-tree-selection
                           load-expanded-paths load-project-map
                           save-expanded-paths save-project-map
                           save-tree-selection get-temp-file)]
        [clooj.utils :only (clooj-prefs write-value-to-prefs read-value-from-prefs
                            is-mac count-while get-coords add-text-change-listener
                            set-selection scroll-to-pos add-caret-listener
                            attach-child-action-keys attach-action-keys
                            get-caret-coords add-menu make-undoable
                            choose-file choose-directory
                            comment-out uncomment-out
                            indent unindent)])
  (:require [clojure.contrib.string :as string]
            [clojure.main :only (repl repl-prompt)])
  (:gen-class))


(def gap 5)

(def docs (atom {}))
  
(def mono-font
  (if (is-mac)
    (Font. "Monaco" Font/PLAIN 11)
    (Font. "Courier New" Font/PLAIN 12)))

(defn make-text-area []
  (doto (JTextArea.)
    (.setFont mono-font)))  
      
  

;; caret finding

(defn display-caret-position [doc]
  (let [{:keys [row col]} (get-caret-coords (:doc-text-area doc))]
    (.setText (:pos-label doc) (str " " (inc row) "|" (inc col)))))

;; highlighting

(defn activate-caret-highlighter [text-comp]
  (add-caret-listener text-comp #(highlight-caret-enclosure text-comp)))

(defn activate-error-highlighter [text-comp]
  (add-text-change-listener
    text-comp 
    #(do (.. text-comp getHighlighter removeAllHighlights)
             (highlight-bad-brackets text-comp))))

;; paren closing (doesn't work)

(defn close-bracket [text-area] ;; doesn't work yet
  (let [text (.getText text-area)
        right-pos (.getCaretPosition text-area)
        left-pos (find-left-enclosing-bracket text right-pos)
        left-char (.charAt text left-pos)]
    (if-let [right-char (get {\( \), \{ \}, \[ \]} left-char)]
      (.insert text-area (str right-char) right-pos))))

;; temp files

(defn dump-temp-doc [doc orig-f txt]
  (try 
    (when orig-f
   ;   (println "dumping..." orig-f)
      (let [orig (.getAbsolutePath orig-f)
            f (.getAbsolutePath (get-temp-file orig-f))]
         (spit f txt)
         (SwingUtilities/invokeLater #(.updateUI (doc :docs-tree)))))
    (catch Exception e (println e orig-f))))

(def temp-file-manager (agent 0))

(defn update-temp [doc]
  (let [txt (.getText (doc :doc-text-area))
        f @(doc :file)]
   ; (println "update-temp")
    (send-off temp-file-manager
      #(let [now (System/currentTimeMillis)]
         (if (> (- now %) 10000)
           (do (dump-temp-doc doc f txt) now)
           %)))))

(defn setup-temp-writer [doc]
  (add-text-change-listener (:doc-text-area doc) #(update-temp doc)))

(declare restart-doc)

(defn setup-tree [doc]
  (let [tree (:docs-tree doc)
        save #(save-expanded-paths tree)]
    (doto tree
      (.setModel (DefaultTreeModel. (DefaultMutableTreeNode. "projects")))
      (.setRootVisible false)
      (.setShowsRootHandles true)
      (.. getSelectionModel (setSelectionMode TreeSelectionModel/SINGLE_TREE_SELECTION))
      (.addTreeExpansionListener
        (reify TreeExpansionListener
          (treeCollapsed [this e] (save))
          (treeExpanded [this e] (save))))
      (.addTreeSelectionListener
        (reify TreeSelectionListener
          (valueChanged [this e]
           ; (println e)
            (save-tree-selection tree (.getNewLeadSelectionPath e))
            (let [f (.. e getPath getLastPathComponent
                          getUserObject)]
              (when (.. f getName (endsWith ".clj"))
                (SwingUtilities/invokeLater #(restart-doc doc f))))))))))

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

(defn auto-indent-str [text-comp offset]
  (let [bracket-pos (find-left-enclosing-bracket
                      (.getText text-comp) offset)]
    (if (<= 0 bracket-pos)
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

(defn make-split-pane [comp1 comp2 horizontal resize-weight]
  (doto (JSplitPane. (if horizontal JSplitPane/HORIZONTAL_SPLIT 
                                    JSplitPane/VERTICAL_SPLIT)
                     true comp1 comp2)
        (.setResizeWeight resize-weight)
        (.setOneTouchExpandable false)
        (.setBorder (BorderFactory/createEmptyBorder))
        (.setDividerSize gap)))

(defn setup-search-text-area [doc]
  (let [sta (doto (doc :search-text-area)
      (.setVisible false)
      (.setFont mono-font)
      (.setBorder (BorderFactory/createLineBorder Color/DARK_GRAY))
      (.addFocusListener (proxy [FocusAdapter] [] (focusLost [_] (stop-find doc)))))]
    (add-text-change-listener sta #(update-find-highlight doc false))
    (attach-action-keys sta ["ENTER" #(highlight-step doc false)]
                            ["shift ENTER" #(highlight-step doc true)]
                            ["ESCAPE" #(escape-find doc)])))

(defn get-shape [doc]
  (let [f (doc :frame)]
        {:x (.getX f) :y (.getY f) :w (.getWidth f) :h (.getHeight f)
         :dsp (.getDividerLocation (doc :doc-split-pane))
         :rsp (.getDividerLocation (doc :repl-split-pane))
         :sp (.getDividerLocation (doc :split-pane))}))

(defn set-shape [doc shape]
  (let [{:keys [x y w h dsp rsp sp]} shape]
    (.setBounds (doc :frame) x y w h)
    (.setDividerLocation (doc :doc-split-pane) dsp)
    (.setDividerLocation (doc :repl-split-pane) rsp)
    (.setDividerLocation (doc :split-pane) sp)))

(defn save-shape [doc]
  (write-value-to-prefs clooj-prefs "shape" (get-shape doc)))

(defn load-shape [doc]
  (when-let [shape (read-value-from-prefs clooj-prefs "shape")]
    (set-shape doc shape)))

(defn create-doc []
  (let [doc-text-area (make-text-area)
        doc-text-panel (JPanel.)
        repl-out-text-area (make-text-area)
        repl-out-writer (make-repl-writer repl-out-text-area)
        repl-in-text-area (make-text-area)
        search-text-area (JTextField.)
        pos-label (JLabel.)
        f (JFrame.)
        cp (.getContentPane f)
        layout (SpringLayout.)
        docs-tree (JTree.)
        doc-split-pane (make-split-pane
                         (make-scroll-pane docs-tree)
                         doc-text-panel true 0)
        repl-split-pane (make-split-pane
                          (make-scroll-pane repl-out-text-area)
                          (make-scroll-pane repl-in-text-area) false 0.75)
        split-pane (make-split-pane doc-split-pane repl-split-pane true 0.5)
        doc {:doc-text-area doc-text-area
             :repl-out-text-area repl-out-text-area
             :repl-in-text-area repl-in-text-area
             :frame f
             :docs-tree docs-tree
             :search-text-area search-text-area
             :pos-label pos-label :file (atom nil)
             :repl-out-writer repl-out-writer
             :repl (atom (create-clojure-repl repl-out-writer))
             :doc-split-pane doc-split-pane
             :repl-split-pane repl-split-pane
             :split-pane split-pane
             :changed false}
        doc-scroll-pane (make-scroll-pane doc-text-area)]
    (doto f
      (.setBounds 25 50 950 700)
      (.setLayout layout)
      (.add split-pane))   
    (doto doc-text-panel
      (.setLayout (SpringLayout.))
      (.add doc-scroll-pane)
      (.add pos-label)
      (.add search-text-area))
    (doto pos-label
      (.setFont (Font. "Courier" Font/PLAIN 13)))
    (constrain-to-parent split-pane :n gap :w gap :s (- gap) :e (- gap))
    (constrain-to-parent doc-scroll-pane :n 0 :w 0 :s -16 :e 0)
    (constrain-to-parent pos-label :s -16 :w 0 :s 0 :w 100)
    (constrain-to-parent search-text-area :s -16 :w 100 :s -1 :w 300)
    (.layoutContainer layout f)
    (setup-search-text-area doc)
    (add-caret-listener doc-text-area #(display-caret-position doc))
    (activate-caret-highlighter doc-text-area)
    (activate-caret-highlighter repl-in-text-area)
    (doto repl-out-text-area (.setLineWrap true) (.setEditable false))
    (make-undoable repl-in-text-area)
    (set-tab-as-spaces repl-in-text-area 2)
    (activate-error-highlighter repl-in-text-area)
    (setup-tree doc)
    doc))

;; clooj docs

(defn get-project [^File file]
  (

(defn restart-doc [doc ^File file] 
  (send-off temp-file-manager
    (let [f @(:file doc)
          txt (.getText (:doc-text-area doc))]        
      (fn [_] (when (and f (.exists (get-temp-file f)))
             ;   (println "about to dump: "f)
                (dump-temp-doc doc f txt))
              0)))
  (let [frame (doc :frame)]
    (let [text-area (doc :doc-text-area)
          temp-file (get-temp-file file)
          file-to-open (if (.exists temp-file) temp-file file)]
      (if file-to-open
        (do (.read text-area (FileReader. file-to-open) nil)
            (.setTitle frame (str "clooj  \u2014  " (.getPath file))))
        (do (.setText text-area "")
            (.setTitle frame "Untitled")))
      (make-undoable text-area)
      (set-tab-as-spaces text-area 2)
      (activate-error-highlighter text-area)
      (reset! (doc :file) file)
      (setup-temp-writer doc)
      (apply-namespace-to-repl doc))))

(defn open-file [doc]
  (let [frame (doc :frame)]
    (when-let [file (choose-file frame "Open a clojure source file" ".clj" true)]
      (restart-doc doc file))))

(defn new-file [doc]
  (restart-doc doc nil))

(declare save-file-as)

(defn save-file [doc]
  (if-not @(doc :file)
    (save-file-as doc)
    (let [f @(doc :file)
          ft (File. (str (.getAbsolutePath f) "~"))]
      (.write (doc :doc-text-area) (FileWriter. f))
      (send-off temp-file-manager (fn [_] 0))
      (.delete ft)
      (.updateUI (doc :docs-tree)))))

(defn save-file-as [doc]
  (let [frame (doc :frame)
        file (choose-file frame "Save a clojure source file" ".clj" false)]
    (reset! (doc :file) file)
    (if @(doc :file)
     (save-file doc)
     (.setTitle frame (.getPath file)))))

(defn open-project [doc]
  (let [dir (choose-directory (doc :f) "Choose a project directory")
        project-dir (if (= (.getName dir) "src") (.getParentFile dir) dir)]
    (add-project-to-tree doc (.getAbsolutePath project-dir)))
  (save-project-map))

(defn make-menus [doc]
  (System/setProperty "apple.laf.useScreenMenuBar" "true")
  (let [menu-bar (JMenuBar.)]
    (. (doc :frame) setJMenuBar menu-bar)
    (add-menu menu-bar "File"
      ["New" "cmd N" #(new-file doc)]
      ["Open" "cmd O" #(open-file doc)]
      ["Open project..." "cmd shift O" #(open-project doc)]
      ["Save" "cmd S" #(save-file doc)]
      ["Save as..." "cmd shift S" #(save-file-as doc)])
    (add-menu menu-bar "Source"
      ["Comment-out" "cmd SEMICOLON" #(comment-out (:doc-text-area doc))]
      ["Uncomment-out" "cmd shift SEMICOLON" #(uncomment-out (:doc-text-area doc))])
    (add-menu menu-bar "REPL"
      ["Evaluate nearest root form" "cmd ENTER" #(send-selected-to-repl doc)]
      ["Evaluate entire file" "cmd E" #(send-doc-to-repl doc)]
      ["Apply file ns" "cmd L" #(apply-namespace-to-repl doc)]
      ["Clear output" "cmd K" #(.setText (doc :repl-out-text-area) "")]
      ["Restart" "cmd R" #(restart-repl doc)])
    (add-menu menu-bar "Search"
      ["Find" "cmd F" #(start-find doc)]
      ["Find next" "cmd G" #(highlight-step doc false)]
      ["Find prev" "cmd shift G" #(highlight-step doc true)]
      )))

;; startup

(def current-doc (atom nil))

(defn startup []
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [doc (create-doc)]
     (reset! current-doc doc)
     (make-menus doc)
     (let [ta-in (doc :repl-in-text-area)
           ta-out (doc :repl-out-text-area)]
       (add-repl-input-handler doc))
     (doall (map #(add-project-to-tree doc %) (keys (load-project-map))))
     (load-shape doc)
     (.setVisible (doc :frame) true)
     (add-line-numbers (doc :doc-text-area) Short/MAX_VALUE)
     (setup-temp-writer doc)
     (let [tree (doc :docs-tree)]
       (load-expanded-paths tree)
       (load-tree-selection tree))))

(defn -show []
  (if (not @current-doc)
    (startup)
    (.setVisible (:frame current-doc) true)))

(defn -main [& args]
  (startup))

;; testing

(defn get-text []
  (.getText (current-doc :doc-text-area)))
