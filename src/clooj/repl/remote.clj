(ns clooj.repl.remote
  (:import (java.io StringReader)))

(def silence (atom true))

(defmacro silent
  "Silently evaluate code in a repl such that
   it is omitted from the *1,*2,*3 history."
  [& body]
  `(do
     (reset! clooj.repl.remote/silence true)
     ~@body
     (let [last-val# *1]
       (set! *1 *2)
       (set! *2 *3)
       last-val#)))

(defn read-code-at
  "Read some text as code, as though it were located
   at a particular line number."
  [text line]
  (read (proxy [clojure.lang.LineNumberingPushbackReader]
          [(StringReader. (str "(do " text ")"))]
          (getLineNumber []
                         (+ -1 line (proxy-super getLineNumber))))))
    
(defn eval-code-at
  "Evaluate some text as code, as though it were located
   in a given file at a particular line number."
  [text file line]
  (binding [*file* file]
    (eval (read-code-at text line))))

(defn repl
  "Starts a REPL (for nesting in a primitive REPL) that
   prints nicely and suppresses silent evaluations."
  []
  (clojure.main/repl
    :print (fn [x]
             (if @silence
               (do
                 (reset! silence false)
                 (println))
               (if (var? x)
                 (println x)
                 (clojure.pprint/pprint x))))
    :prompt #(do (clojure.main/repl-prompt) (.flush *out*))))