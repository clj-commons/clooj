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
    (:import 
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel))
    (:require [clooj.rsyntax :as rsyntax]))


(defn make-text-editor-comp
  [app-atom]
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
                                      :id             :doc-scrollable
                                      :class          :text-editor-comp)
        doc-text-panel        (vertical-panel       
                                      :items         [doc-label 
                                                      doc-scroll-pane 
                                                      position-search-panel]
                                      :id             :doc-text-panel
                                      :class          :text-editor-comp)]
    (swap! app-atom conj (gen-map
                            arglist-label
                            search-text-area
                            arg-search-panel
                            pos-label
                            position-search-panel 
                            doc-label
                            doc-text-area
                            doc-scroll-pane
                            doc-text-panel))
    doc-text-panel))

(defn make-file-tree-comp
  [app-atom]
  (let [docs-tree             (tree   :model          (DefaultTreeModel. nil)
                                      :id             :file-tree
                                      :class          :file-tree)
        docs-tree-scroll-pane (scrollable             docs-tree
                                      :id             :file-tree-scrollable
                                      :class          :file-tree)
        docs-tree-label       (border-panel 
                                      :west           (label "Projects")
                                      :size           [200 :by 15]
                                      :vgap           5
                                      :id             :file-tree-label
                                      :class          :file-tree)
        docs-tree-panel (vertical-panel 
                                      :items          [docs-tree-label 
                                                      docs-tree-scroll-pane]
                                      :id             :file-tree-panel
                                      :class          :file-tree)]
    (swap! app-atom conj (gen-map
                            docs-tree
                            docs-tree-scroll-pane
                            docs-tree-label
                            docs-tree-panel))
      docs-tree-panel))

(defn make-repl-comp
  [app-atom]
  (let [repl-out-text-area  (rsyntax/text-area 
                                      :wrap-lines?    false
                                      :editable?      false
                                      :id             :repl-out-text-area
                                      :class          :repl)
        repl-out-writer   (make-repl-writer repl-out-text-area)
        repl-out-scroll-pane (scrollable repl-out-text-area                                      
                                      :id             :repl-out-scrollable
                                      :class          :repl)
        repl-output-vertical-panel (vertical-panel 
                                      :items          [repl-out-scroll-pane]                                      
                                      :id             :repl-output-vertical-panel
                                      :class          :repl)
        repl-in-text-area (rsyntax/text-area 
                                      :wrap-lines?    false
                                      :syntax         "clojure"                                      
                                      :id             :repl-in-text-area
                                      :class          :repl)
        repl-input-vertical-panel (vertical-panel 
                                      :items          [repl-in-text-area]                                      
                                      :id             :repl-input-vertical-panel
                                      :class          :repl)
        repl-split-pane (top-bottom-split             repl-output-vertical-panel 
                                                      repl-input-vertical-panel
                                      :divider-location 0.66
                                      :resize-weight 0.66
                                      :divider-size   3)]
    (swap! app-atom conj (gen-map
                            repl-out-scroll-pane
                            repl-out-text-area
                            repl-in-text-area
                            repl-input-vertical-panel
                            repl-out-writer
                            repl-split-pane))
      repl-split-pane))

(defn make-doc-nav-comp
  [app-atom]
  (let [completion-label (label       :text           "Name search"
                                      :id             :doc-nav-label
                                      :class          :doc-nav-comp)
        completion-list (listbox      :border         (compound-border "Doc List")
                                      :id             :doc-nav-list
                                      :class          :doc-nav-comp)
        completion-scroll-pane (scrollable            completion-list
                                      :id             :doc-nav-scrollable
                                      :class          :doc-nav-comp)
        completion-panel (vertical-panel 
                                      :items          [completion-label 
                                                      completion-scroll-pane]
                                      :id             :doc-nav-panel
                                      :class          :doc-nav-comp)]
    (swap! app-atom conj (gen-map
                            completion-label
                            completion-list
                            completion-scroll-pane
                            completion-panel))

    completion-panel))

(defn make-doc-view-comp
  [app-atom]
  (let [help-text-area (rsyntax/text-area  
                                      :wrap-lines?    true
                                      :editable?      false
                                      :background     (color 0xFF 0xFF 0xE8)
                                      :border         (compound-border "Documentation")
                                      :id             :doc-view-text-area
                                      :class          :doc-view-comp)
        help-text-scroll-pane (scrollable             help-text-area
                                      :id             :doc-view-scrollable
                                      :class          :doc-view-comp)]
    (swap! app-atom conj (gen-map
                            help-text-area
                            help-text-scroll-pane))  
    help-text-scroll-pane))

(defn add-behaviors
  [app]
    ;; docs
    (setup-completion-list (app :completion-list) app)    
    (setup-tab-help app (app :doc-text-area))
    ;;editor
    (setup-autoindent (app :doc-text-area))
    (doto (app :doc-text-area) attach-navigation-keys)
    (double-click-selector (app :doc-text-area))
    (add-caret-listener (app :doc-text-area) #(display-caret-position app))
    (setup-search-text-area app)
    (activate-caret-highlighter app)
    (setup-temp-writer app)
    (attach-action-keys (app :doc-text-area)
      ["cmd1 ENTER" #(send-selected-to-repl app)])
    ;; repl
    (setup-autoindent (app :repl-in-text-area))
    (setup-tab-help app (app :repl-in-text-area))
    (doto (app :repl-in-text-area)
            double-click-selector
            attach-navigation-keys)
    ;; global
    (dorun (map #(attach-global-action-keys % app)
                [(app :docs-tree) 
                 (app :doc-text-area) 
                 (app :repl-in-text-area) 
                 (app :repl-out-text-area) 
                 (.getContentPane (app :frame))]))
    ;; frame
    )
    
(defn create-app []
  (let [app-init  (atom {})

        editor    (make-text-editor-comp app-init)
        file-tree (make-file-tree-comp app-init)
        repl      (make-repl-comp app-init)

        doc-view  (make-doc-view-comp app-init)
        doc-nav   (make-doc-nav-comp app-init)

        doc-split-pane (left-right-split
                         file-tree
                         editor
                         :divider-location 0.25
                         :resize-weight 0.25
                         :divider-size 5)
        split-pane (left-right-split 
                        doc-split-pane 
                        repl
                        :divider-location 0.66
                        :resize-weight 0.66
                        :divider-size 5)
        frame (frame 
                :title "Clooj" 
                :width 950 
                :height 700 
                :on-close :exit
                :minimum-size [500 :by 350]
                :content split-pane)

        app (merge {:file      (atom nil)
                    :repl      (atom (create-outside-repl (@app-init :repl-out-writer) nil))
                    :changed   false}
                    @app-init
                    (gen-map
                      frame
                      doc-split-pane
                      split-pane))]
    (add-behaviors app)
    app))

(defonce current-app (atom nil))

(defn -show []
  (reset! embedded true)
  (if (not @current-app)
    (startup create-app current-app)
    (.setVisible (:frame @current-app) true)))

(defn -main [& args]
  (reset! embedded false)
  (reset! current-app (create-app))
  (add-behaviors @current-app)
  (invoke-later
    (-> 
      (startup @current-app) 
      show!)))




