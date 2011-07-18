; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.tree 
  (:import (java.io File)
           (java.awt GridLayout)
           (javax.swing JButton JTree JOptionPane JWindow)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel))
  (:use [clooj.utils :only (clooj-prefs read-value-from-prefs
                            write-value-to-prefs awt-event
                            choose-file)]))

;; projects tree

(declare restart-doc)

(def project-set (atom (sorted-set)))

(defn save-project-set []
  (write-value-to-prefs clooj-prefs "project-set" @project-set))
    
(defn load-project-set []
  (reset! project-set (into (sorted-set)
                        (read-value-from-prefs clooj-prefs "project-set"))))

(defn get-root-path [tree]
  (TreePath. (.. tree getModel getRoot)))

(defn tree-path-to-file [^TreePath tree-path]
  (when tree-path
    (try (.. tree-path getLastPathComponent getUserObject getAbsolutePath)
      (catch Exception e nil))))
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

(defn project-set-to-tree-model []
   (let [model (DefaultTreeModel. (DefaultMutableTreeNode. "projects"))]
     (doseq [project @project-set]
       (let [src-path (str project File/separator "src")
             src-file (File. src-path)
             project-clj-path (str project File/separator "project.clj")
             root (.getRoot model)
             project (add-node root (.getName (File. project)) project)]
           (when (.exists (File. project-clj-path))
             (add-node project "project.clj" project-clj-path))
           (when (and (.exists src-file) (not (empty? (.listFiles src-file))))
             (add-srcs-to-src-node (add-node project "src" src-path) src-path))))
     model))

(defn update-project-tree [tree]
  (let [model (project-set-to-tree-model)]
    (awt-event
      (.setModel tree model)
      (save-project-set)
      (load-expanded-paths tree)
      (save-expanded-paths tree))))

(defn get-project-node [tree node]
  (let [parent-node (.getParent node)]
    (if (= parent-node
           (.getLastPathComponent (get-root-path tree)))
      node
      (get-project-node tree (.getParent node)))))

(defn get-node-path [node]
  (.. node getUserObject getAbsolutePath))

(defn get-selected-file-path [doc]
  (-> doc
    :docs-tree .getSelectionPaths first
    .getLastPathComponent .getUserObject .getAbsolutePath))

(defn get-selected-projects [doc]
  (let [tree (doc :docs-tree)
        selections (.getSelectionPaths tree)]
    (for [selection selections]
      (->> selection .getLastPathComponent (get-project-node tree)
                     .getUserObject .getAbsolutePath))))

(defn add-project [doc project-path]
  (swap! project-set conj project-path)
  (update-project-tree (doc :docs-tree)))

(defn rename-project [doc]
  (when-let [dir (choose-file (doc :frame) "Move/rename project directory" "" false)]
    (let [old-project (first (get-selected-projects doc))]
      (if (.renameTo (File. old-project) dir)
        (do
          (swap! project-set
                 #(-> % (disj old-project) (conj (.getAbsolutePath dir))))
          (update-project-tree (:docs-tree doc)))
        (JOptionPane/showMessageDialog nil "Unable to move project.")))))

(defn remove-selected-project [doc]
  (apply swap! project-set disj (get-selected-projects doc))
  (update-project-tree (doc :docs-tree)))     
      
