; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.core
    (:use [seesaw core graphics color border]
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
           (org.fife.ui.rtextarea RTextScrollPane))
    (:require [clooj.rsyntax :as rsyntax]))

(native!)

(defn make-text-editor-comp
  [app]
  (let [arglist-label         (label  :foreground     (color :blue)
                                      :id             :arglist-label
                                      :class          :arg-response)
        search-text-area      (text   :id             :search-text-area
                                      :class          :search-area)
        arg-search-panel      (horizontal-panel 
                                      :items          [arglist-label search-text-area]
                                      :id             :arg-search-panel
                                      :class          :search-panel)
        pos-label             (label  :id             :pos-label
                                      :class          :pos-label)
        position-search-panel (horizontal-panel 
                                      :items          [pos-label 
                                                     [:fill-h 10]
                                                       arg-search-panel
                                                      :fill-h]
                                      :maximum-size   [2000 :by 15]
                                      :id             :position-search-panel
                                      :class          :search-panel)
        doc-label             (label  :text           "Source Editor"
                                      :id             :doc-label
                                      :class          :text-editor-comp)
        doc-text-area         (rsyntax/text-area    
                                      :wrap-lines?    false
                                      :id             :doc-text-area
                                      :class          :text-editor-comp)
        doc-scroll-pane       (scrollable              doc-text-area
                                      :id             :doc-scroll-pane
                                      :class          :text-editor-comp)
        doc-text-panel        (vertical-panel       
                                      :items         [doc-label 
                                                      doc-scroll-pane 
                                                      position-search-panel]
                                      :id             :doc-text-panel
                                      :class          :text-editor-comp)]
    [doc-text-panel
      (merge 
        app
        (gen-map
          arglist-label
          search-text-area
          arg-search-panel
          pos-label
          position-search-panel 
          doc-label
          doc-text-area
          doc-scroll-pane
          doc-text-panel))]))

; (defn make-doc-tree-comp
;   [app]
;   (let [docs-tree             (tree   :model (DefaultTreeModel. nil))
;         docs-tree-scroll-pane (scrollable docs-tree)
;         docs-tree-label       (border-panel 
;                                       :west (label "Projects")
;                                       :size [200 :by 15]
;                                       :vgap 5)
;         docs-tree-panel (vertical-panel 
;                             :items [docs-tree-label 
;                                     docs-tree-scroll-pane])]))


(defn create-app []
  (let [
        app-init (second (-> {} make-text-editor-comp))


        help-text-area (rsyntax/text-area  :wrap-lines? true
                                           :editable? false
                                           :background (color 0xFF 0xFF 0xE8)
                                           :border (compound-border "Documentation"))
        help-text-scroll-pane (scrollable help-text-area)
        completion-label (label "Name search")

        completion-list (listbox :border (compound-border "Doc List"))
        
        completion-scroll-pane (scrollable completion-list)
        completion-panel (vertical-panel :items [completion-label completion-scroll-pane])

        docs-tree (tree :model (DefaultTreeModel. nil))
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
                         (app-init :doc-text-panel)
                         :divider-location 0.25
                         :resize-weight 0.25
                         :divider-size 5)


        repl-out-text-area (rsyntax/text-area :wrap-lines? false
                                              :editable? false
                                              :id :repl-out-text-area
                                              :class :repl)
        repl-out-writer (make-repl-writer repl-out-text-area)
        repl-out-scroll-pane (scrollable repl-out-text-area)
        repl-output-vertical-panel (vertical-panel :items [repl-out-scroll-pane])
        repl-in-text-area (rsyntax/text-area :wrap-lines? false
                                             :syntax "clojure")
        repl-input-vertical-panel (vertical-panel :items [repl-in-text-area])
        repl-split-pane (top-bottom-split 
                            repl-output-vertical-panel 
                            repl-input-vertical-panel
                            :divider-location 0.66
                            :divider-size 5)


        split-pane (left-right-split 
                        doc-split-pane 
                        repl-split-pane 
                        :divider-location 0.66
                        :resize-weight 0.66
                        :divider-size 5)
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
                    app-init
                   (gen-map
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
                     repl-out-writer
                     doc-split-pane
                     repl-split-pane
                     split-pane
                     completion-list
                     completion-scroll-pane
                     completion-panel))]

    (doto (app :doc-text-area)
      attach-navigation-keys)
    (setup-completion-list completion-list app)
    (double-click-selector (app :doc-text-area))
    (doto repl-in-text-area
      double-click-selector
      attach-navigation-keys)
    (setup-search-text-area app)
    (add-caret-listener (app :doc-text-area) #(display-caret-position app))
    (activate-caret-highlighter app)
    (setup-temp-writer app)
    (attach-action-keys (app :doc-text-area)
      ["cmd1 ENTER" #(send-selected-to-repl app)])
    (setup-autoindent repl-in-text-area)
    (setup-tab-help app (app :doc-text-area))
    (dorun (map #(attach-global-action-keys % app)
                [docs-tree (app :doc-text-area) repl-in-text-area repl-out-text-area (.getContentPane frame)]))
    
    (setup-autoindent (app :doc-text-area))
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



