; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.tree 
  (:import (java.io File)
           (javax.swing JTree)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel))
  (:use [clooj.utils :only (clooj-prefs read-value-from-prefs
                            write-value-to-prefs)]))


;; projects tree

(declare restart-doc)

(def project-map (atom nil))

(defn save-project-map []
  (write-value-to-prefs clooj-prefs "project-map" @project-map))
    
(defn load-project-map []
  (reset! project-map (read-value-from-prefs clooj-prefs "project-map")))

(defn get-root-path [tree]
  (TreePath. (.. tree getModel getRoot)))

(comment (defn get-tree-nodes [tree]
  (let [get-nodes     
         (fn [root-node]
           (cons root-node
             (map get-nodes
                  (enumeration-seq (.children root-node)))))]
    (get-nodes (.. tree getModel getRoot)))))

(defn tree-path-to-file [^TreePath tree-path]
  (when tree-path
    (.. tree-path getLastPathComponent getUserObject getAbsolutePath)))

(defn get-row-path [tree row]
  (tree-path-to-file (. tree getPathForRow row)))

(defn get-expanded-paths [tree]
  (for [i (range (.getRowCount tree)) :when (.isExpanded tree i)]
    (get-row-path tree i)))

(defn expand-paths [tree paths]
  (doseq [i (range) :while (< i (.getRowCount tree))]
    (when-let [x (some #{(tree-path-to-file (. tree getPathForRow i))} paths)]
      (.expandPath tree (. tree getPathForRow i)))))

(defn save-expanded-paths [tree]
  (write-value-to-prefs clooj-prefs "expanded-paths" (get-expanded-paths tree)))

(defn load-expanded-paths [tree]
  (let [paths (read-value-from-prefs clooj-prefs "expanded-paths")]
    (when paths
      (expand-paths tree paths))))

(defn save-tree-selection [tree path]
  (write-value-to-prefs
    clooj-prefs "tree-selection"
    (tree-path-to-file path)))
  
(defn load-tree-selection [tree]
  (let [path (read-value-from-prefs clooj-prefs "tree-selection")]
    (doseq [row (range (.getRowCount tree))]
      (when (= path (get-row-path tree row)) (.setSelectionRow tree row)))))

(defn get-project-root [path]
  (let [f (File. path)
        name (.getName f)]
    (if (and (or (= name "src")
                 (= name "lib"))
             (.isDirectory f))
      (File. (.getParent f))
      f)))

(defn get-code-files [dir suffix]
  (let [dir (File. dir)]
    (filter #(.endsWith (.getName %) suffix)
            (file-seq dir))))

(defn path-to-namespace [file-path]
  (let [drop-suffix #(clojure.contrib.string/butlast 4 %)]
    (-> file-path
        (.split (str "src" (.replace File/separator "\\" "\\\\")))
        second
        drop-suffix
        (.replace File/separator "."))))

(defn get-temp-file [^File orig]
  (when orig
    (File. (str (.getAbsolutePath orig) "~"))))

(defn file-node [text file-path] 
  (proxy [File] [file-path]
    (toString []
      (let [mark (if (.exists (get-temp-file this)) "*" "")]
        (str mark text mark)))))

(defn add-node [parent node-str file-path]
  (let [node  (DefaultMutableTreeNode.
                (file-node node-str file-path))]
    (.add parent node)
    node))

(defn add-code-file-to-src-node [src-node code-file]
  (let [f (.getAbsolutePath code-file)
        namespace (path-to-namespace f)]
        (add-node src-node namespace f)))

(defn add-srcs-to-src-node [src-node src-dir]
  (doall (map #(add-code-file-to-src-node src-node %)
              (get-code-files src-dir ".clj"))))

(defn add-project-to-tree [doc project-root]
  (let [model (.getModel (doc :docs-tree))
        src-path (str project-root File/separator "src")]
    (-> model
      .getRoot
      (add-node (.getName (File. project-root)) project-root)
      (add-node "src" src-path)
      (add-srcs-to-src-node src-path))
    (.. model reload))
  (swap! project-map assoc project-root nil))

(defn get-project-node [tree node]
  (let [parent-node (.getParent node)]
    (if (= parent-node
           (.getLastPathComponent (get-root-path tree)))
      node
      (get-project-node tree (.getParent node)))))

(defn remove-project [tree node]
  (.removeNodeFromParent (.getModel tree) node)
  (swap! project-map dissoc (.. node getUserObject getAbsolutePath))
  (save-project-map))

(defn remove-selected-project [doc]
  (println "remove selected proejct")
  (let [tree (doc :docs-tree)
        selections (.getSelectionPaths tree)]
    (doseq [selection selections]
      (->> selection .getLastPathComponent
                     (get-project-node tree)
                     (remove-project tree)))))
      
      

