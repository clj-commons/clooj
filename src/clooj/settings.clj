; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.settings
  (:import 
    (javax.swing JFrame JTabbedPane JLabel
                 JPanel JComboBox Box
                 JTextField JTextArea 
                 BoxLayout SpringLayout
                 JButton JCheckBox)
    (java.awt Font GraphicsEnvironment Dimension)
    (java.awt.image BufferedImage)
    (javax.swing.event DocumentListener)
    (java.awt.event ActionListener ItemListener ItemEvent))
  (:require
    [clooj.utils :as utils]))

(def settings (atom nil))

(defn combo-box [items default-item change-fun]
  (doto (JComboBox. (into-array items))
    (.setSelectedItem default-item)
    (.addActionListener
      (reify ActionListener
        (actionPerformed [_ e]
          (change-fun (.. e getSource getSelectedItem)))))))

(defn text-field [default-value change-fun]
  (let [tf (JTextField. (str default-value))]
    (.addDocumentListener
      (.getDocument tf)
      (reify DocumentListener
        (insertUpdate [_ e] 
                      (change-fun (.getText tf)))
        (removeUpdate [_ e]
                      (change-fun (.getText tf)))
        (changedUpdate [_ e])))
    tf))

(defn check-box [text checked? change-fun]
  (doto (JCheckBox. text checked?)
    (.addItemListener
      (reify ItemListener
        (itemStateChanged [_ e]
          (change-fun 
            (= 
              (.getStateChange e) 
              ItemEvent/SELECTED)))))))

(defn font-panel []
  
  
  (def graphics-object
    (memoize (fn [] (.createGraphics
                      (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB)))))
  
  (def monospaced?
    (fn [font]
      (let [g (graphics-object)
            m (.getFontMetrics g font)]
        (apply == (map #(.charWidth m %) [\m \n \. \M \-])))))

  (defn get-all-font-names []
    (.. GraphicsEnvironment
        getLocalGraphicsEnvironment
        getAvailableFontFamilyNames))
    
  (defn get-all-fonts-12 []
    (map #(Font. % Font/PLAIN 12) (get-all-font-names)))
  
  (defn get-monospaced-font-names []
    (map #(.getName %) (filter monospaced? (get-all-fonts-12))))
  
  (defn get-necessary-fonts []
    (if (:show-only-monospaced-fonts @settings)
      (get-monospaced-font-names)
      (get-all-font-names)))
  (let [example-text-area (doto (JTextArea. 
                                  "abcdefghijklmnopqrstuvwxyz 0123456789 (){}[]\nABCDEFGHIJKLMNOPQRSTUVWXYZ +-*/= .,;:!? #&$%@|^")
                            (.setFont (Font. (:font-name @settings) Font/PLAIN (:font-size @settings))))
        example-pane (doto (JPanel. (SpringLayout.))      
                       (.add example-text-area))
        font-box (combo-box
                   (get-necessary-fonts)
                   (:font-name @settings)
                   #(do 
                      (swap! settings assoc :font-name %)
                      (.setFont 
                        example-text-area 
                        (Font. % Font/PLAIN (:font-size @settings)))))
        size-box (combo-box
                   (range 5 49)
                   (:font-size @settings)
                   #(do 
                      (swap! settings assoc :font-size %)
                      (.setFont 
                        example-text-area 
                        (Font. (:font-name @settings) Font/PLAIN %))))
        monospaced-check-box (check-box
                               "Show only monospaced fonts"
                               (:show-only-monospaced-fonts @settings)
                               #(do
                                  (swap! settings 
                                         assoc :show-only-monospaced-fonts %)
                                  (doto font-box 
                                    (.setModel 
                                      (.getModel 
                                        (JComboBox.
                                          (into-array 
                                            (get-necessary-fonts)))))
                                    (.setSelectedItem (:font-name @settings)))))
        controls-pane (JPanel.)
        font-pane (JPanel.)]

    (utils/constrain-to-parent example-text-area :n 20 :w 15 :s -15 :e -15)
    
    (doto controls-pane
      (.setLayout (BoxLayout. controls-pane BoxLayout/X_AXIS))
      (.add (Box/createRigidArea (Dimension. 20 0))) 
      (.add (JLabel. "Font:"))
      (.add (Box/createRigidArea (Dimension. 25 0)))    
      (.add font-box)
      (.add (Box/createRigidArea (Dimension. 25 0)))  
      (.add (JLabel. "Size:"))
      (.add (Box/createRigidArea (Dimension. 25 0)))  
      (.add size-box)
      (.add (Box/createHorizontalGlue)))
    
    (doto font-pane
      (.setLayout (BoxLayout. font-pane BoxLayout/Y_AXIS))
      (.add controls-pane)
      (.add monospaced-check-box)
      (.add example-pane))))

(defn editor-options-panel []
  (let [options-pane (JPanel.)]
    (doto options-pane
      (.setLayout (BoxLayout. options-pane BoxLayout/Y_AXIS))
      (.add (check-box 
              "Wrap lines in source editor" 
              (:line-wrap-doc @settings)
              #(swap! settings assoc :line-wrap-doc %)))
      (.add (check-box 
              "Wrap lines in repl output" 
              (:line-wrap-repl-out @settings)
              #(swap! settings assoc :line-wrap-repl-out %)))
      (.add (check-box 
              "Wrap lines in repl input" 
              (:line-wrap-repl-in @settings)
              #(swap! settings assoc :line-wrap-repl-in %))))))
  
(defmacro tabs [& elements]
  `(doto (JTabbedPane.)
     ~@(map #(list '.addTab (first %) (second %)) elements)))

(defn make-settings-window [app apply-fn]
  (let [bounds (.getBounds (:frame app))
        x (+ (.x bounds) (/ (.width bounds) 2))
        y (+ (.y bounds) (/ (.height bounds) 2))
        settings-frame (JFrame. "Settings")
        button-pane (JPanel.)]
    
    (doto button-pane
      (.setLayout (BoxLayout. button-pane BoxLayout/X_AXIS))
      (.add (utils/create-button "OK" #(do
                                        (apply-fn app @settings)
                                        (.dispose settings-frame))))
      (.add (utils/create-button "Apply" #(apply-fn app @settings)))
      (.add (utils/create-button "Cancel" #(.dispose settings-frame))))
    
    (doto settings-frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setLayout (BoxLayout. (.getContentPane settings-frame) BoxLayout/Y_AXIS))
      (.setBounds (- x 250) (- y 250) 500 500)

      (.add (tabs
              ["Font" (font-panel)]
              ["Editor options" (editor-options-panel)]))
      (.add (Box/createRigidArea (Dimension. 0 25)))
      (.add button-pane))))
             
              
            
(defn show-settings-window [app apply-fn]
  (reset! settings @(:settings app))
  (.show (make-settings-window app apply-fn)))
