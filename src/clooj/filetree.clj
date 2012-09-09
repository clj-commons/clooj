(ns clooj.filetree
    (:import (java.io File)
           (java.awt GridLayout)
           (javax.swing JButton JTree JOptionPane JWindow)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (javax.swing.event TreeExpansionListener))
    (:use [clooj.utils :only (clooj-prefs read-value-from-prefs)]
          [clojure.java.io :only (file)]))

(defn get-projects
  []
  (->> (read-value-from-prefs clooj-prefs "project-set")
      set
      (sort-by #(.toLowerCase (.getName (file %))))
      vec))

(defn file-node
  "Tree node representing a file (possibly a directory)."
  [^File file]
  (let [children (vec (.listFiles file))]
    (proxy [DefaultMutableTreeNode] [file]
      (getChildAt [i] (file-node (children i)))
      (getChildCount [] (count (.listFiles file)))
      (toString [] (.getName file))
      (isLeaf [] (not (.isDirectory file))))))

(defn root-node
  "The root tree node, given a vector of project locations."
  [projects]
  (proxy [DefaultMutableTreeNode] []
    (getChildAt [i] (file-node (File. (nth projects i))))
    (getChildCount [] (count projects))
    (toString [] "root")))

(defn file-tree []
  (doto
    (JTree. (DefaultTreeModel. (root-node (get-projects)) false))
    (.setRootVisible false)
    (.setShowsRootHandles true)))

;; test


(defn test-tree 
  "Make a Window and put the file-tree in it."
  [tree]
  (let [f (javax.swing.JFrame.)
        sp (javax.swing.JScrollPane. tree)]
    (doto f
      (-> .getContentPane (.add sp))
      (.setBounds 10 10 300 600)
      .show)))