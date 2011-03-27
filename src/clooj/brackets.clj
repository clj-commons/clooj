; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.brackets
  (:import (javax.swing.text JTextComponent))
  (:require [clojure.contrib.string :as string])
  (:use [clooj.utils :only (count-while)]))


;; bracket handling

(defn bracket-score [c]
  (condp = c 
         \(  1 \[  1 \{  1
         \) -1 \] -1 \} -1
         0))

(defn bracket-increment [score next-char]
  (+ score (bracket-score next-char)))

(defn count-brackets [s]
  (reductions bracket-increment 0 s))

(defn find-left-enclosing-bracket [text pos]
  (let [before (string/take pos text)]
    (- pos (count-while
             (partial >= 0)
             (count-brackets (string/reverse before))))))

(defn find-right-enclosing-bracket [text pos]
  (let [after (string/drop pos text)]
    (+ -1 pos (count-while
                (partial <= 0)
                (count-brackets after)))))

(defn find-enclosing-brackets [text pos]
  [(find-left-enclosing-bracket text pos)
   (find-right-enclosing-bracket text pos)])

(defn mismatched-brackets [a b]
  (and (or (nil? a) (some #{a} [\( \[ \{]))
       (some #{b} [\) \] \}])
       (not (some #{[a b]} [[\( \)] [\[ \]] [\{ \}]]))))

(defn find-bad-brackets [text]
  (loop [t text cnt 0 stack nil errs nil]
    (let [s stack
          c (first t)
          l (ffirst s)
          p (next s)
          j (conj s [c cnt])
          new-stack
            (condp = l
              \\ p
              \" (if (= c \") p s)
              \; (if (= c \newline) p s)
              (condp = c
                \" j \\ j \; j
                \( j \[ j \{ j
                \) p \] p \} p
                s))
          e (if (mismatched-brackets l c)
              (list (first s) [c cnt]))
          new-errs (if e (concat errs e) errs)]
        (if (next t)
          (recur (next t) (inc cnt) new-stack new-errs)
          (filter identity
                  (map second (concat new-stack errs)))))))

(defn find-enclosing-root-form [text-comp pos]
  (let [l (.. text-comp getDocument getLength)
        text (.getText text-comp)
        right-brackets
          (next (take-while #(> l %) 
            (iterate #(inc (find-right-enclosing-bracket text %)) pos)))
        left-brackets
          (next (take-while #(< 0 %)
            (iterate #(find-left-enclosing-bracket text %) pos)))]
    (let [lb (last left-brackets) lr (last right-brackets)]
    (when (and lb lr (= (count left-brackets) (count right-brackets)))
      [lb lr]))))

(defn find-nearby-root-form [text-comp]
  (let [pos (.getCaretPosition text-comp)
        f #(find-enclosing-root-form text-comp %)]
    (or (f pos) (f (dec pos)) (f (inc pos)))))
