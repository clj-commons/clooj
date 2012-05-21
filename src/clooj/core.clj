; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.core
 (:use [seesaw core graphics color]
        [clojure.pprint :only (pprint)]
        [clooj.dev-tools]
        [clooj.brackets]
        [clooj.highlighting]
        [clooj.repl]
        [clooj.search]
        [clooj.help]
        [clooj.project]
        [clooj.utils]
        [clooj.indent]
        [clooj.style]
        [clooj.navigate])
  (:import (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree KeyStroke SpringLayout JTextPane
                        ListSelectionModel
                        UIManager)
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (java.awt Insets Rectangle Window)
           (java.awt.event AWTEventListener FocusAdapter MouseAdapter
                           WindowAdapter KeyAdapter)
           (java.awt AWTEvent Color Font GridLayout Toolkit)
           (java.net URL)
           (java.util Map)
           (java.io File FileReader StringReader
                    BufferedWriter OutputStreamWriter FileOutputStream)
           (org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants TokenMakerFactory)  
           (org.fife.ui.rtextarea RTextScrollPane)))


(defn create-app []
  (let [
        arglist-label (label)
        
        search-text-area (text :size [200 :by 12])
        pos-label (label :font (font "COURIER" 13))
        position-search-panel (border-panel 
                                :west pos-label 
                                :center search-text-area
                                :hgap 15)

        doc-label (label "Source Editor")        
        doc-text-area (make-text-area false)
        doc-scroll-pane (make-scroll-pane doc-text-area)
        doc-text-panel (vertical-panel :items [doc-label doc-scroll-pane position-search-panel])
        
        help-text-area (make-text-area true)
        help-text-scroll-pane (scrollable help-text-area)

        completion-label (label "Name search")
        completion-list (listbox )
        completion-scroll-pane (scrollable completion-list)
        completion-panel (vertical-panel :items [completion-label completion-scroll-pane])

        cp (:content-pane frame)

        docs-tree (tree)
        docs-tree-scroll-pane (scrollable docs-tree)
        docs-tree-label (border-panel 
                          :west (label "Projects")
                          :size [200 :by 15]
                          :vgap 5)
        docs-tree-panel (vertical-panel 
                            :items [docs-tree-label 
                                    docs-tree-scroll-pane])

        doc-split-pane (left-right-split
                         docs-tree-panel
                         doc-text-panel
                         :divider-location 0.2)

        repl-out-text-area (make-text-area false)
        repl-out-writer (make-repl-writer repl-out-text-area)
        
        repl-out-scroll-pane (scrollable repl-out-text-area)
        repl-output-vertical-panel (vertical-panel :items [repl-out-scroll-pane])

        repl-in-text-area (make-text-area false)
        repl-input-vertical-panel (vertical-panel :items [repl-in-text-area])

        repl-split-pane (top-bottom-split 
                            repl-output-vertical-panel 
                            repl-input-vertical-panel
                            :divider-location 0.7)
                
        split-pane (top-bottom-split 
                        doc-split-pane 
                        repl-split-pane 
                        :divider-location 0.7)

        frame (frame 
                :title "Overtone sketch" 
                :width 950 
                :height 700 
                :on-close :exit
                :minimum-size [500 :by 350]
                :content split-pane)

        app (merge {:file (atom nil)
                    :repl (atom (create-outside-repl repl-out-writer nil))
                    :changed false}
                   (gen-map
                     doc-text-area
                     doc-label
                     repl-out-text-area
                     repl-in-text-area
                     frame
                     help-text-area
                     help-text-scroll-pane
                     repl-out-scroll-pane
                     docs-tree
                     docs-tree-scroll-pane
                     docs-tree-panel
                     docs-tree-label
                     search-text-area
                     pos-label
                     repl-out-writer
                     doc-split-pane
                     repl-split-pane
                     split-pane
                     arglist-label
                     completion-list
                     completion-scroll-pane
                     completion-panel))]


    
    (doto doc-text-area
      attach-navigation-keys)
    
    (setup-completion-list completion-list app)

    
    (double-click-selector doc-text-area)
    
    (doto repl-in-text-area
      double-click-selector
      attach-navigation-keys)

    (.setSyntaxEditingStyle repl-in-text-area
                            SyntaxConstants/SYNTAX_STYLE_CLOJURE)

    (.setModel docs-tree (DefaultTreeModel. nil))

    ; (exit-if-closed frame)

    (setup-search-text-area app)
    
    (add-caret-listener doc-text-area #(display-caret-position app))
    
    (activate-caret-highlighter app)
    
    (setup-temp-writer app)
    
    (attach-action-keys doc-text-area
      ["cmd1 ENTER" #(send-selected-to-repl app)])
    
    (doto repl-out-text-area (.setEditable false))
    
    (doto help-text-area (.setEditable false)
                         (.setBackground (color 0xFF 0xFF 0xE8)))
    
    (setup-autoindent repl-in-text-area)
    
    (setup-tab-help app doc-text-area)
    
    (dorun (map #(attach-global-action-keys % app)
                [docs-tree doc-text-area repl-in-text-area repl-out-text-area (.getContentPane frame)]))
    
    (setup-autoindent doc-text-area)
    app))

(defonce current-app (atom nil))

(defn -show []
  (reset! embedded true)
  (if (not @current-app)
    (startup create-app current-app)
    (.setVisible (:frame @current-app) true)))

(defn -main [& args]
  (reset! embedded false)
  (startup create-app current-app))



