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

(defn blank-line-matcher [s]
  (re-matcher #"[\n\r]\s*?[\n\r]" s))

(defn find-left-gap [text pos]
  (let [p (min (.length text) (inc pos))
        before-reverse (string/reverse (string/take p text))
        matcher (blank-line-matcher before-reverse)]
    (if (.find matcher)
      (- p (.start matcher))
      0)))

(defn find-right-gap [text pos]
  (let [p (max 0 (dec pos))
        after (string/drop p text)
        matcher (blank-line-matcher after) ]
    (if (.find matcher)
      (+ p (.start matcher))
      (.length text))))

(defn find-line-group [text-comp]
  (let [text (.getText text-comp)
        pos (.getCaretPosition text-comp)]
    [(find-left-gap text pos) (find-right-gap text pos)]))