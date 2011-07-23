(ns clooj.help
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT Reflector))
  (:use [clooj.brackets :only (find-enclosing-brackets)])
  (:require [clojure.contrib.string :as string]))

; from http://clojure.org/special_forms
(def special-forms
  {"def" "(def symbol init?)"
   "if"  "(if test then else?)"
   "do"  "(do exprs*)"
   "let" "(let [bindings* ] exprs*)"
   "quote" "(quote form)"
   "var" "(var symbol)"
   "fn"  "(fn name? [params* ] exprs*)"
   "loop" "(loop [bindings* ] exprs*)"
   "recur" "(recur exprs*)"
   "throw" "(throw expr)"
   "try"   "(try expr* catch-clause* finally-clause?)"
   "catch" "(catch classname name expr*)"
   "monitor-enter" "Avoid!"
   "monitor-exit"  "Avoid!"})
 

(defmacro with-ns
  "Evaluates body in another namespace.  ns is either a namespace
  object or a symbol.  This makes it possible to define functions in
  namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     ~@(map (fn [form] `(eval '~form)) body)))

(defn var-source [v]
  (when-let [filepath (:file (meta v))]
    (when-let [strm (.getResourceAsStream (RT/baseLoader) filepath)]
      (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
        (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
        (let [text (StringBuilder.)
              pbr (proxy [PushbackReader] [rdr]
                    (read [] (let [i (proxy-super read)]
                               (.append text (char i))
                               i)))]
          (read (PushbackReader. pbr))
          (str text))))))

(defn var-help [v]
  (when-let [m (meta v)]
    (let [d (:doc m)
          s  (or (:clooj/src m)
                 (var-source v))]
       (str (:name m)
            (if (:ns m) (str " [" (:ns m) "]") "") "\n"
            (:arglists m) "\n"
            (when d (str d "\n\n"))
            (when s
              (if d
                (.replace s d "...docs...")
                d))))))

(defn find-form-string [text pos]
  (let [[left right] (find-enclosing-brackets text pos)]
    (when (> (.length text) left)
      (.substring text (inc left)))))

(defn head-token [form-string]
  (when form-string
    (second
      (re-find #"(.*?)[\s|\)|$]"
               (str (.trim form-string) " ")))))

(defn string-to-var [ns string]
  (when (and ns string)
    (try
      (ns-resolve ns (symbol string))
      (catch Exception e))))

(defn form-help [ns form-string]
  (var-help (ns-resolve ns (head-token form-string))))

(defn arglist-from-var [v]
  (or
    (when-let [m (meta v)]
      (when-let [args (:arglists m)]
        (str (:name m) ": " args)))
    ""))

(defn token-from-caret-pos [ns text pos]
  (head-token (find-form-string text pos)))

(defn arglist-from-caret-pos [ns text pos]
  (let [token (token-from-caret-pos ns text pos)]
    (or (special-forms token)
        (arglist-from-var (string-to-var ns token)))))

(defn var-help-from-caret-pos [ns text pos]
  (or
    (when (and ns text pos)
      (var-help (var-from-caret-pos ns text pos)))
    ""))