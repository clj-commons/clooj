; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.utils
  (:import (java.util UUID)
           (java.awt FileDialog Point Window)
           (java.awt.event ActionListener MouseAdapter)
           (java.util.prefs Preferences)
           (java.security MessageDigest)
           (java.io ByteArrayInputStream ByteArrayOutputStream
                    File FilenameFilter
                    ObjectInputStream ObjectOutputStream
                    OutputStream Writer PrintStream)
           (javax.swing AbstractAction JButton JFileChooser JMenu JMenuBar JMenuItem BorderFactory
                        JOptionPane JSplitPane KeyStroke SpringLayout SwingUtilities)
           (javax.swing.event CaretListener DocumentListener UndoableEditListener)
           (javax.swing.undo UndoManager)))

;; general

(defmacro do-when [f & args]
  (let [args_ args]
    `(when (and ~@args_)
       (~f ~@args_))))

(defmacro when-lets [bindings & body]
  (assert (vector? bindings))
  (let [n (count bindings)]
    (assert (zero? (mod n 2)))
    (assert (<= 2 n))
  (if (= 2 n)
    `(when-let ~bindings ~@body)
    (let [[a b] (map vec (split-at 2 bindings))]     
      `(when-let ~a (when-lets ~b ~@body))))))

(defn count-while [pred coll]
  (count (take-while pred coll)))

(defn remove-nth [s n]
  (lazy-cat (take n s) (drop (inc n) s)))

(defmacro awt-event [& body]
  `(SwingUtilities/invokeLater (fn [] ~@body)))

(defmacro gen-map [& args]
  (let [kw (map keyword args)]
    (zipmap kw args)))

;; preferences
  
;; define a UUID for clooj preferences
(def clooj-prefs (.. Preferences userRoot
                     (node "clooj") (node "c6833c87-9631-44af-af83-f417028ea7aa")))

(defn partition-str [n s]
  (let [l (.length s)]
    (for [i (range 0 l n)]
      (.substring s i (Math/min l (+ (int i) (int n))))))) 

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
  (when-not (.endsWith key "/")
    (let [node (. prefs node key)]
      (let [s (apply str
                     (for [i (range (count (. node keys)))]
                       (.get node (str i) nil)))]
        (when (and s (pos? (.length s))) (read-string s))))))

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

(defn get-line-of-offset [text-pane offset]
  (.. text-pane getDocument getDefaultRootElement (getElementIndex offset)))
  
(defn get-line-start-offset [text-pane line]
  (.. text-pane getDocument getDefaultRootElement (getElement line) getStartOffset))

(defn get-line-end-offset [text-pane line]
  (.. text-pane getDocument getDefaultRootElement (getElement line) getEndOffset))

(defn get-line-text [text-pane line]
  (let [start (get-line-start-offset text-pane line)
        length (- (get-line-end-offset text-pane line) start)]
    (.. text-pane getDocument (getText start length))))

(defn append-text [text-pane text]
  (when-let [doc (.getDocument text-pane)]
    (.insertString doc (.getLength doc) text nil)))

(defn get-coords [text-comp offset]
  (let [row (get-line-of-offset text-comp offset)
        col (- offset (get-line-start-offset text-comp row))]
    {:row row :col col}))

(defn get-caret-coords [text-comp]
  (get-coords text-comp (.getCaretPosition text-comp)))

(defn add-text-change-listener [text-comp f]
  "Executes f whenever text is changed in text component."
  (.addDocumentListener
    (.getDocument text-comp)
    (reify DocumentListener
      (insertUpdate [this evt] (f))
      (removeUpdate [this evt] (f))
      (changedUpdate [this evt]))))

(defn remove-text-change-listeners [text-comp]
  (let [d (.getDocument text-comp)]
    (doseq [l (.getDocumentListeners d)]
      (.removeDocumentListener d l))))
                               
(defn get-text-str [text-comp]
  (let [doc (.getDocument text-comp)]
    (.getText doc 0 (.getLength doc))))

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
    (when r
      (.setViewPosition v
                        (Point. 0 (min (- l h) (max 0 (- (.y r) (/ h 2)))))))))

(defn scroll-to-caret [text-comp]
  (scroll-to-pos text-comp (.getCaretPosition text-comp)))

(defn focus-in-text-component [text-comp]
  (.requestFocusInWindow text-comp)
  (scroll-to-caret text-comp))

(defn get-selected-lines [text-comp]
  (let [row1 (get-line-of-offset text-comp (.getSelectionStart text-comp))
        row2 (inc (get-line-of-offset text-comp (.getSelectionEnd text-comp)))]
    (doall (range row1 row2))))

(defn get-selected-line-starts [text-comp]
  (map #(get-line-start-offset text-comp %)
       (reverse (get-selected-lines text-comp))))

(defn insert-in-selected-row-headers [text-comp txt]
  (awt-event
    (let [starts (get-selected-line-starts text-comp)
          document (.getDocument text-comp)]
      (dorun (map #(.insertString document % txt nil) starts)))))

(defn remove-from-selected-row-headers [text-comp txt]
  (awt-event
    (let [len (count txt)
          document (.getDocument text-comp)]
      (doseq [start (get-selected-line-starts text-comp)]
        (when (= (.getText (.getDocument text-comp) start len) txt)
          (.remove document start len))))))
  
(defn comment-out [text-comp]
  (insert-in-selected-row-headers text-comp ";"))

(defn uncomment-out [text-comp]
  (remove-from-selected-row-headers text-comp ";"))
    
(defn indent [text-comp]
  (when (.isFocusOwner text-comp)
    (insert-in-selected-row-headers text-comp " ")))

(defn unindent [text-comp]
  (when (.isFocusOwner text-comp)
    (remove-from-selected-row-headers text-comp " ")))

;; other gui

(defn make-split-pane [comp1 comp2 horizontal divider-size resize-weight]
  (doto (JSplitPane. (if horizontal JSplitPane/HORIZONTAL_SPLIT 
                                    JSplitPane/VERTICAL_SPLIT)
                     true comp1 comp2)
        (.setResizeWeight resize-weight)
        (.setOneTouchExpandable false)
        (.setBorder (BorderFactory/createEmptyBorder))
        (.setDividerSize divider-size)))

;; keys

(defn get-keystroke [key-shortcut]
  (KeyStroke/getKeyStroke
    (-> key-shortcut
      (.replace "cmd1" (if (is-mac) "meta" "ctrl"))
      (.replace "cmd2" (if (is-mac) "ctrl" "alt")))))

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

(defn attach-action-keys
  "Maps input keys to action-fns."
  [comp & items]
  (doall (map #(apply attach-action-key comp %) items)))
  
;; buttons
 
(defn create-button [text fn]
  (doto (JButton. text)
    (.addActionListener
      (reify ActionListener
        (actionPerformed [_ _] (fn))))))

;; menus

(defn add-menu-item
  ([menu item-name key-mnemonic key-accelerator response-fn]
    (let [menu-item (JMenuItem. item-name)]  
      (when key-accelerator
        (.setAccelerator menu-item (get-keystroke key-accelerator)))
      (when (and (not (is-mac)) key-mnemonic)
        (.setMnemonic menu-item (.getKeyCode (get-keystroke key-mnemonic))))
      (.addActionListener menu-item
                          (reify ActionListener
                            (actionPerformed [this action-event]
                                             (response-fn))))
      (.add menu menu-item)))
  ([menu item]
    (condp = item
      :sep (.addSeparator menu))))
  
(defn add-menu
  "Each item-tuple is a vector containing a
  menu item's text, mnemonic key, accelerator key, and the function
  it executes."
  [menu-bar title key-mnemonic & item-tuples]
  (let [menu (JMenu. title)]
    (when (and (not (is-mac)) key-mnemonic)
      (.setMnemonic menu (.getKeyCode (get-keystroke key-mnemonic))))
    (doall (map #(apply add-menu-item menu %) item-tuples))
    (.add menu-bar menu)
    menu))

;; mouse

(defn on-click [comp num-clicks fun]
  (.addMouseListener comp
    (proxy [MouseAdapter] []
      (mouseClicked [event]
        (when (== num-clicks (.getClickCount event))
          (.consume event)
          (fun))))))

;; undoability

(defn make-undoable [text-area]
  (let [undoMgr (UndoManager.)]
    (.setLimit undoMgr 1000)
    (.. text-area getDocument (addUndoableEditListener
        (reify UndoableEditListener
          (undoableEditHappened [this evt] (.addEdit undoMgr (.getEdit evt))))))
    (attach-action-keys text-area
      ["cmd1 Z" #(if (.canUndo undoMgr) (.undo undoMgr))]
      ["cmd1 shift Z" #(if (.canRedo undoMgr) (.redo undoMgr))])))


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
    (let [fc (JFileChooser.)
          last-open-dir (read-value-from-prefs clooj-prefs "last-open-dir")]
      (doto fc (.setFileSelectionMode JFileChooser/DIRECTORIES_ONLY)
               (.setDialogTitle title)
               (.setCurrentDirectory (if last-open-dir (File. last-open-dir) nil)))
       (if (= JFileChooser/APPROVE_OPTION (.showOpenDialog fc parent))
         (.getSelectedFile fc)))))

;; tree seq on widgets (awt or swing)

(defn widget-seq [^java.awt.Component comp]
  (tree-seq #(instance? java.awt.Container %)
            #(seq (.getComponents %))
            comp))

;; saving and restoring window shape in preferences

(defn get-shape [components]
  (for [comp components]
    (condp instance? comp
      Window
        [:window {:x (.getX comp) :y (.getY comp)
                  :w (.getWidth comp) :h (.getHeight comp)}]
      JSplitPane
        [:split-pane {:location (.getDividerLocation comp)}]
      nil)))

(defn watch-shape [components fun]
  (doseq [comp components]
    (condp instance? comp
      Window
        (.addComponentListener comp
          (proxy [java.awt.event.ComponentAdapter] []
            (componentMoved [_] (fun))
            (componentResized [_] (fun))))
      JSplitPane
        (.addPropertyChangeListener comp JSplitPane/DIVIDER_LOCATION_PROPERTY
          (proxy [java.beans.PropertyChangeListener] []
            (propertyChange [_] (fun))))
      nil)))

(defn set-shape [components shape-data]
  (loop [comps components shapes shape-data]
    (let [comp (first comps)
          shape (first shapes)]
      (try
        (when shape
          (condp = (first shape)
            :window
            (let [{:keys [x y w h]} (second shape)]
              (.setBounds comp x y w h))
            :split-pane
            (.setDividerLocation comp (:location (second shape)))
            nil))
        (catch Exception e nil)))
    (when (next comps)
      (recur (next comps) (next shapes)))))

(defn save-shape [prefs name components]
  (write-value-to-prefs prefs name (get-shape components)))

(defn restore-shape [prefs name components]
  (try
    (set-shape components (read-value-from-prefs prefs name))
    (catch Exception e)))
    
(defn confirmed? [question title]
  (= JOptionPane/YES_OPTION
     (JOptionPane/showConfirmDialog
       nil question title  JOptionPane/YES_NO_OPTION)))

(defn persist-window-shape [prefs name ^java.awt.Window window]
  (let [components (widget-seq window)
        shape-persister (agent nil)]
    (restore-shape prefs name components)
    (watch-shape components
                 #(send-off shape-persister
                            (fn [old-shape]
                              (let [shape (get-shape components)]
                                (when (not= old-shape shape)
                                  (write-value-to-prefs prefs name shape))
                                shape))))))

(defn sha1-str [obj]
   (let [bytes (.getBytes (with-out-str (pr obj)))] 
     (String. (.digest (MessageDigest/getInstance "MD") bytes))))

;; streams and writers
 
(defn printstream-to-writer [writer]
  (->
    (proxy [OutputStream] []
      (write
        ([^bytes bs offset length]
          (.write writer
                  (.toCharArray (String. ^bytes bs "utf-8"))
                  offset length))
        ([b]
          (.write writer b)))
      (flush [] (.flush writer))
      (close [] (.close writer)))
    (PrintStream. true)))
