(ns clooj.help
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT Reflector)
           (java.awt Point)
           (java.util Vector))
  (:use [clooj.brackets :only (find-enclosing-brackets)]
        [clooj.repl :only (get-current-namespace)]
        [clooj.utils :only (attach-action-keys awt-event when-lets)]
        [clojure.repl :only (source-fn)])
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
                   (source-fn name))]
       (str (:name m)
            (if (:ns m) (str " [" (:ns m) "]") "") "\n"
            (:arglists m)
            "\n\n"
            (if d
              (str "Documentation:\n" d)
              "No documentation found.")
            "\n\n"
            (if s
              (str "Source:\n"
                   (if d
                     (.replace s d "...docs...")
                     s))
              "No source found.")))))

(defn find-form-string [text pos]
  (let [[left right] (find-enclosing-brackets text pos)]
    (when (> (.length text) left)
      (.substring text (inc left)))))

(def non-token-chars [\( \) \[ \] \{ \} \  \newline \" \'])

(defn local-token [text pos]
  (let [start (loop [p (dec pos)]
                (if (some #{(.charAt text p)} non-token-chars)
                  (inc p)
                  (recur (dec p))))
        stop (loop [p pos]
               (if (some #{(.charAt text p)} non-token-chars)
                 p
                 (recur (inc p))))]
    (.substring text start stop)))

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

(defn token-help [ns token]
  (when (pos? (.length token))
  (var-help (try (ns-resolve (symbol ns) (symbol token))
                 (catch ClassNotFoundException e nil)))))

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

(def help-visible (atom false))

(defn set-first-component [split-pane comp]
  (let [loc (.getDividerLocation split-pane)]
    (.setTopComponent split-pane comp)
    (.setDividerLocation split-pane loc)))

(defn ns-symbols [ns]
  (concat
    (sort (keys (ns-interns ns)))
    (sort (keys (ns-refers ns)))))

(defn present-help-list [app ns token]
  (let [l (app :completion-list)
        symbols (map str (-> ns symbol ns-symbols))
        best-symbols (sort (filter #(.startsWith % token) symbols))
        others (sort (filter #(.contains (.substring % 1) token) symbols))]
    (.setListData l (Vector. (concat best-symbols others)))
    (.setSelectedIndex l 0)))

(defn show-tab-help [app text-comp]
  (let [ns (get-current-namespace text-comp)
        text (.getText text-comp)
        pos (.getCaretPosition text-comp)]
    (awt-event
      (when-let [token (local-token text pos)]
        (print token)
        (let [help-txt (or (token-help ns token) "")]
          (.setText (app :help-text-area) help-txt))
        (present-help-list app ns token)
        (set-first-component (app :repl-split-pane)
                             (app :help-text-scroll-pane))
        (set-first-component (app :doc-split-pane)
                             (app :completion-scroll-pane))
        (.ensureIndexIsVisible (app :completion-list) 0)
        (awt-event 
          (-> app :help-text-scroll-pane .getViewport (.setViewPosition (Point. (int 0) (int 0))))
          (-> app :completion-scroll-pane .getViewport (.setViewPosition (Point. (int 0) (int 0))))
          (reset! help-visible true))))))

(defn hide-tab-help [app]
  (when @help-visible
    (awt-event
      (set-first-component (app :repl-split-pane)
                           (app :repl-out-scroll-pane))
      (set-first-component (app :doc-split-pane)
                           (app :docs-tree-scroll-pane))
      (reset! help-visible false))))
  
(defn setup-tab-help [app text-comp]
  (attach-action-keys text-comp
    ["TAB" #(show-tab-help app text-comp)]
    ["ESCAPE" #(hide-tab-help app)]))
