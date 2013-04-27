(ns clooj.repl.output
  (:import (java.awt Point Rectangle)
           (java.util.concurrent.atomic AtomicBoolean)
           (javax.swing JFrame JScrollPane JSplitPane JSlider JTextArea
                        SwingUtilities)))


(defn scroll-to-end
  "Moves the caret to the last line of a text-area.
   Causes text-area to scroll to the end."
  [text-area]
  (->> text-area
       .getLineCount
       dec
       (.getLineStartOffset text-area)
       (.setCaretPosition text-area)))
    
(defn tailing-text-area
  "Creates a JTextArea in a JScrollPane that scrolls
   to the bottom whenever text is appended."
  []
  (let [should-scroll (AtomicBoolean. false)
        text-area (proxy [JTextArea] []
                    (append [^String text]
                            (proxy-super append text)
                            (.set should-scroll true)))
        scroll-pane (proxy [JScrollPane] [text-area]
                      (paint [graphics]
                             (when (.getAndSet should-scroll false)
                               (scroll-to-end text-area))
                             (proxy-super paint graphics)))]
    {:text-area text-area
     :scroll-pane scroll-pane}))

;; manual tests

(defn test-text-area
  "Creates a JTextArea, shows it in a JFrame with a
   JSlider above it. Returns the text-area instance."
  []
  (let [{:keys [text-area scroll-pane]} (tailing-text-area)
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

