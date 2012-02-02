; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.help
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT Reflector)
           (java.lang.reflect Modifier)
           (java.awt Color Point)
           (java.util Vector)
           (javax.swing DefaultListCellRenderer ListSelectionModel)
           (javax.swing.event ListSelectionListener))
  (:use [clooj.brackets :only (find-enclosing-brackets)]
        [clooj.repl :only (get-file-ns get-repl-ns)]
        [clooj.utils :only (attach-action-keys attach-child-action-keys
                            on-click awt-event when-lets get-text-str)]
        [clojure.repl :only (source-fn)])
  (:require [clojure.string :as string]))

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

(defn ns-item-name [item]
  (cond
    (var? item) (-> item meta :name str)
    (class? item) (.getSimpleName item)))

(defn ns-item-package [item]
  (cond
    (var? item) (-> item meta :ns)
    (class? item) (.. item getPackage getName)))

(defn present-ns-item [item]
  (str (ns-item-name item) " [" (ns-item-package item) "]"))

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

(defn find-form-string [text pos]
  (let [[left right] (find-enclosing-brackets text pos)]
    (when (> (.length text) left)
      (.substring text (inc left)))))

(def non-token-chars [\; \~ \@ \( \) \[ \] \{ \} \  \. \newline \/ \" \'])

(defn local-token-location [text pos]
  (let [n (.length text)
        pos (-> pos (Math/max 0) (Math/min n))]
    [(loop [p (dec pos)]
       (if (or (neg? p)
               (some #{(.charAt text p)} non-token-chars))
         (inc p)
         (recur (dec p))))
     (loop [p pos]
       (if (or (>= p n)
               (some #{(.charAt text p)} non-token-chars))
         p
         (recur (inc p))))]))

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


(defonce help-state (atom {:visible false :token nil :pos nil}))

(defn var-help [v]
  (when-let [m (meta v)]
    (let [d (:doc m)
          ns (:ns m)
          name (:name m)
          s (binding [*ns* ns]
              (def q "test")
                   (source-fn (symbol (str ns "/" name))))]
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

(defn create-param-list
  ([method-or-constructor static]
    (str " (["
         (let [type-names (map #(.getSimpleName %)
                               (.getParameterTypes method-or-constructor))
               param-names (if static type-names (cons "this" type-names))]
           (apply str (interpose " " param-names)))
         "])"))
  ([method-or-constructor]
    (create-param-list method-or-constructor true)))

(defn constructor-help [constructor]
  (str (.. constructor getDeclaringClass getSimpleName) "."
       (create-param-list constructor)))

(defn method-help [method]
  (let [stat (Modifier/isStatic (.getModifiers method))]
    (str
      (if stat
        (str (.. method getDeclaringClass getSimpleName)
             "/" (.getName method))
        (str "." (.getName method)))
     (create-param-list method stat)
      " --> " (.getName (.getReturnType method)))))

(defn field-help [field]
  (let [c (.. field getDeclaringClass getSimpleName)]
  (str
    (if (Modifier/isStatic (.getModifiers field))
      (str (.. field getDeclaringClass getSimpleName)
           "/" (.getName field)
           (when (Modifier/isFinal (.getModifiers field))
             (str " --> " (.. field (get nil) toString))))
      (str "." (.getName field) " --> " (.getName (.getType field)))))))

(defn class-help [c]
  (apply str
         (concat
           [(present-ns-item c) "\n  java class"]
           ["\n\nCONSTRUCTORS\n"]
           (interpose "\n"
                      (sort
                        (for [constructor (.getConstructors c)]
                          (constructor-help constructor))))
           ["\n\nMETHODS\n"]
           (interpose "\n"
                      (sort
                        (for [method (.getMethods c)]
                          (method-help method))))
           ["\n\nFIELDS\n"]
           (interpose "\n"
                      (sort
                        (for [field (.getFields c)]
                          (field-help field)))))))

(defn item-help [item]
  (cond (var? item) (var-help item)
        (class? item) (class-help item)))    

(defn set-first-component [split-pane comp]
  (let [loc (.getDividerLocation split-pane)]
    (.setTopComponent split-pane comp)
    (.setDividerLocation split-pane loc)))

(defn clock-num [i n]
  (if (zero? n)
    0
    (cond (< i 0) (dec n)
          (>= i n) 0
          :else i)))

(defn list-size [list]
  (-> list .getModel .getSize))

(defn advance-help-list [app ns token index-change-fn]
  (let [local-ns (when ns (symbol ns))
        help-list (app :completion-list)
        token-pat1 (re-pattern (str "(?i)\\A\\Q" token "\\E"))
        token-pat2 (re-pattern (str "(?i)\\Q" token "\\E"))]
    (if (not= token (@help-state :token))
      (do
        (swap! help-state assoc :token token)
        (.setListData help-list (Vector.))
        (when-lets [ns-items (vals (ns-map local-ns))
                    best (sort-by #(.toLowerCase (ns-item-name %))
                                (filter
                                  #(re-find token-pat1 (ns-item-name %))
                                  ns-items))
                    others (sort-by #(.toLowerCase (ns-item-name %))
                                 (filter 
                                   #(re-find token-pat2 (.substring (ns-item-name %) 1))
                                   ns-items))]
                   (.setListData help-list (Vector. (concat best others)))
                   (.setSelectedIndex help-list 0)
                   ))
      (let [n (list-size help-list)]
        (when (pos? n)
          (.setSelectedIndex help-list
                             (clock-num
                               (index-change-fn
                                    (.getSelectedIndex help-list))
                               n)))))
    (when (pos? (list-size help-list))
      (set-first-component (app :repl-split-pane)
                           (app :help-text-scroll-pane))
      (set-first-component (app :doc-split-pane)
                           (app :completion-panel))
      (.setText (app :repl-label) "Documentation")
      (.ensureIndexIsVisible help-list
                             (.getSelectedIndex help-list)))))
  
(defn get-list-token [app]
  (-> app :completion-list .getSelectedValue ns-item-name))

(defn show-help-text [app choice]
  (let [help-text (or (when choice (item-help choice)) "")]
    (.setText (app :help-text-area) help-text))
  (-> app :help-text-scroll-pane .getViewport
      (.setViewPosition (Point. (int 0) (int 0)))))

(defn show-tab-help [app text-comp index-change-fn]
  (awt-event
    (let [ns (condp = text-comp
               (app :doc-text-area) (get-file-ns app)
               (app :repl-in-text-area) (get-repl-ns app))
          text (get-text-str text-comp)
          pos (.getCaretPosition text-comp)
          [start stop] (local-token-location text pos)]
      (when-let [token (.substring text start stop)]
        (swap! help-state assoc :pos start :visible true)
        (when ns
          (advance-help-list app ns token index-change-fn))))))

(defn hide-tab-help [app]
  (awt-event
    (when (@help-state :visible)
      (set-first-component (app :repl-split-pane)
                           (app :repl-out-scroll-pane))
      (set-first-component (app :doc-split-pane)
                           (app :docs-tree-panel))
      (.setText (app :repl-label) "Clojure REPL output"))
    (swap! help-state assoc :visible false :pos nil)))
  
(defn help-handle-caret-move [app text-comp]
  (awt-event
    (when (@help-state :visible)
      (let [[start _] (local-token-location (get-text-str text-comp) 
                                            (.getCaretPosition text-comp))]
        (if-not (= start (@help-state :pos))
          (hide-tab-help app)
          (show-tab-help app text-comp identity))))))

(defn update-token [app text-comp]
  (awt-event
    (let [[start stop] (local-token-location
                         (get-text-str text-comp)
                         (.getCaretPosition text-comp))
          len (- stop start)
          new-token (get-list-token app)]
      (when (and (not (empty? new-token)) (-> app :completion-list
                                              .getModel .getSize pos?))
        (.. text-comp getDocument
            (replace start len new-token nil))))))

(defn setup-tab-help [app text-comp]
  (attach-action-keys text-comp
    ["TAB" #(show-tab-help app text-comp inc)]
    ["shift TAB" #(show-tab-help app text-comp dec)]
    ["ESCAPE" #(hide-tab-help app)])
  (attach-child-action-keys text-comp
    ["ENTER" #(@help-state :visible)
             #(do (hide-tab-help app)
                  (update-token app text-comp))]))

(defn find-focused-text-pane [app]
  (let [t1 (app :doc-text-area)
        t2 (app :repl-in-text-area)]
    (cond (.hasFocus t1) t1
          (.hasFocus t2) t2)))

(defn setup-completion-list [l app]
  (doto l
    (.setBackground (Color. 0xFF 0xFF 0xE8))
    (.setFocusable false)
    (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
    (.setCellRenderer
      (proxy [DefaultListCellRenderer] []
        (getListCellRendererComponent [list item index isSelected cellHasFocus]
          (doto (proxy-super getListCellRendererComponent list item index isSelected cellHasFocus)
            (.setText (present-ns-item item)))))) 
    (.addListSelectionListener
      (reify ListSelectionListener
        (valueChanged [_ e]
          (when-not (.getValueIsAdjusting e)
            (.ensureIndexIsVisible l (.getSelectedIndex l))
            (show-help-text app (.getSelectedValue l))))))
    (on-click 2 #(when-let [text-pane (find-focused-text-pane app)]
                        (update-token app text-pane)))))
