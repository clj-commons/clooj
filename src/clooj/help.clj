(ns clooj.help
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT Reflector))
  (:use [clooj.brackets :only (find-enclosing-brackets)]
        [clooj.repl :only (get-current-namespace)]
        [clooj.utils :only (attach-action-keys awt-event)])
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
          ns (:ns m)
          name (:name m)
          s (binding [*ns* ns]
              (clojure.repl/source-fn name))]
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

(defn safe-resolve [ns string]
  (try
    (ns-resolve ns (symbol string))
    (catch Exception e)))

(defn string-to-var [ns string]
  (when-not (empty? string)
    (let [sym (symbol string)]
      (or (safe-resolve ns sym)
          (safe-resolve (find-ns 'clojure.core) sym)))))

(defn form-help [ns form-string]
  (var-help (ns-resolve (symbol ns) (symbol (head-token form-string)))))

(defn arglist-from-var [v]
  (or
    (when-let [m (meta v)]
      (when-let [args (:arglists m)]
        (str (-> m :ns ns-name) "/" (:name m) ": " args)))
    ""))

(defn token-from-caret-pos [ns text pos]
  (head-token (find-form-string text pos)))

(defn arglist-from-token [ns token]
  (or (special-forms token)
      (arglist-from-var (string-to-var ns token))))

(defn arglist-from-caret-pos [ns text pos]
  (let [token (token-from-caret-pos ns text pos)]
    (arglist-from-token ns token)))

;; tab help

(defn show-tab-help [app text-comp]
  (let [ns (get-current-namespace text-comp)
        text (.getText text-comp)
        pos (.getCaretPosition text-comp)]
    (awt-event
      (.setText (app :help-text-area) (form-help ns (find-form-string text pos)))
      (.setTopComponent (app :repl-split-pane) (app :help-text-scroll-pane)))))

(defn hide-tab-help [app]
  (awt-event
    (.setTopComponent (app :repl-split-pane)
                      (app :repl-out-scroll-pane))))

(defn setup-tab-help [app text-comp]
  (attach-action-keys text-comp
    ["TAB" #(show-tab-help app text-comp)]
    ["ESCAPE" #(hide-tab-help app)]))
