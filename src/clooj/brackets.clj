; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; arthuredelstein@gmail.com

(ns clooj.brackets
  (:import (javax.swing.text JTextComponent))
  (:require [clojure.contrib.string :as string])
  (:use [clooj.utils :only (count-while)]))


;; bracket handling

(defn mismatched-brackets [a b]
  (and (or (nil? a) (some #{a} [\( \[ \{]))
       (some #{b} [\) \] \}])
       (not (some #{[a b]} [[\( \)] [\[ \]] [\{ \}]]))))

(defn process-bracket-stack
  "Receiving a bracket stack s, deal with the next character c
   and datum dat."
  [s c dat]
  (let [l (ffirst s)        ;last char
        p (next s)          ;pop stack
        j (conj s [c dat])] ;conj [char dat] to stack
    (condp = l
      \\ p
      \" (condp = c, \" p, \\ j, s)
      \; (if (= c \newline) p s)
      (condp = c
        \" j \\ j \; j
        \( j \[ j \{ j
        \) p \] p \} p
        s))))

(defn find-right-enclosing-bracket [text pos]
  (let [after (string/drop pos text)]
    (+ -1 pos (count-while identity
                (reductions #(process-bracket-stack %1 %2 nil) '([]) after)))))

(defn find-left-enclosing-bracket [text pos]
  (let [before (string/take pos text)
        scores (map count
                 (reverse
                   (reductions #(process-bracket-stack %1 %2 nil) nil before)))]
    (- pos (count-while #(<= (first scores) %) scores))))

(defn find-enclosing-brackets [text pos]
  [(find-left-enclosing-bracket text pos)
   (find-right-enclosing-bracket text pos)])

(defn find-bad-brackets [text]
  (loop [t text pos 0 stack nil errs nil]
    (let [c (first t)        ;this char
          new-stack (process-bracket-stack stack c pos)
          e (if (mismatched-brackets (ffirst stack) c)
              (list (first stack) [c pos]))
          new-errs (if e (concat errs e) errs)]
        (if (next t)
          (recur (next t) (inc pos) new-stack new-errs)
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