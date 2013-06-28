(ns clooj.repl.output
  (:import (java.awt Point Rectangle)
           (java.util.concurrent.atomic AtomicBoolean AtomicInteger)
           (javax.swing JFrame JScrollPane JSplitPane JSlider JTextArea
                        SwingUtilities)
           (javax.swing.event DocumentEvent DocumentListener)))

(defn end-position
  "Finds the end position of an insert or change in a document
   as reported in a DocumentEvent instance."
  [^DocumentEvent document-event]
  (+ (.getOffset document-event)
     (.getLength document-event)))

(defn tailing-scroll-pane
  "Embeds the given JTextArea in a JScrollPane that scrolls
   to the bottom whenever text is inserted or appended."
  [text-area]
  (let [scroll-offset (AtomicInteger. -1)
        scroll-pane
        (proxy [JScrollPane] [text-area]
         (paintComponent [graphics]
           (let [offset (.getAndSet scroll-offset -1)]
             (when (not= -1 offset)
               (.. this
                   getVerticalScrollBar
                   (setValue (.y (.modelToView text-area offset))))))
           (proxy-super paintComponent graphics)))
        set-scroll-offset (fn [e]
                            (.set scroll-offset (end-position e))
                            (.repaint scroll-pane))]
        (.. text-area getDocument
            (addDocumentListener
              (proxy [DocumentListener] []
                (changedUpdate [e] (set-scroll-offset e))
                (insertUpdate [e] (set-scroll-offset e))
                (removeUpdate [e]))))
    scroll-pane))
  
;; manual tests

(defn test-text-area
  "Creates a JTextArea, shows it in a JFrame with a
   JSlider above it. Returns the text-area instance."
  []
  (let [text-area (JTextArea.)
        scroll-pane (tailing-scroll-pane text-area)
        ;[text-area scroll-pane] (tailing-text-area)
        frame (JFrame. "test")
        document (.getDocument text-area)
        slider (JSlider. 0 100)
        split-pane (JSplitPane. JSplitPane/VERTICAL_SPLIT
                                true slider scroll-pane)]
    (doto (.getContentPane frame)
      (.add split-pane))
    (doto frame
      .pack
      (.setBounds 30 30 400 400)
      .show)
    text-area
    ))
                         
(defn write-lines
  "Write n lines of text (positive integers) in 
   the text-area"
  [text-area n]
  (dotimes [i n]
    (.append text-area (str i "\n"))))

