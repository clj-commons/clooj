(ns clooj.filetree
    (:import (java.io File)
           (java.awt GridLayout)
           (javax.swing JButton JTree JOptionPane JWindow)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (javax.swing.event TreeExpansionListener)))



(defn file-node [^File file]
  (let [children (vec (.listFiles file))]
    (proxy [DefaultMutableTreeNode] [file]
      (getChildAt [i] (file-node (children i)))
      (getChildCount [] (count (.listFiles file)))
      (toString [] (.getName file))
      (isLeaf [] (not (.isDirectory file))))))

(defn file-tree [root-file]
  (JTree. (DefaultTreeModel. (file-node (File. root-file)) false)))

;; test

(import javax.swing.JFrame)
(import javax.swing.JScrollPane)


(let [t (file-tree "/projects")
      f (JFrame.)
      sp (JScrollPane. t)]
  (doto f
    (-> .getContentPane (.add sp))
    (.show)))