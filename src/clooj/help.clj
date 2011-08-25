(ns clooj.help
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT Reflector)
           (java.awt Color Point)
           (java.util Vector)
           (javax.swing DefaultListCellRenderer ListSelectionModel)
           (javax.swing.event ListSelectionListener))
  (:use [clooj.brackets :only (find-enclosing-brackets)]
        [clooj.repl :only (get-file-ns get-repl-ns)]
        [clooj.utils :only (attach-action-keys attach-child-action-keys
                            on-click awt-event when-lets)]
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

(defn find-form-string [text pos]
  (let [[left right] (find-enclosing-brackets text pos)]
    (when (> (.length text) left)
      (.substring text (inc left)))))

(def non-token-chars [\; \~ \@ \( \) \[ \] \{ \} \  \newline \" \'])

(defn local-token-location [text pos]
  [(loop [p (dec pos)]
     (if (or (neg? p)
             (some #{(.charAt text p)} non-token-chars))
       (inc p)
       (recur (dec p))))
   (let [n (.length text)]
     (loop [p pos]
       (if (or (>= p n)
               (some #{(.charAt text p)} non-token-chars))
         p
         (recur (inc p)))))])

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

(defn set-first-component [split-pane comp]
  (let [loc (.getDividerLocation split-pane)]
    (.setTopComponent split-pane comp)
    (.setDividerLocation split-pane loc)))

(defn ns-vars [ns]
  (concat
    (vals (ns-interns ns))
    (vals (ns-refers ns))))

(defn clock-num [i n]
  (if (zero? n)
    0
    (cond (< i 0) (dec n)
          (>= i n) 0
          :else i)))

(defn list-size [list]
  (-> list .getModel .getSize))

(defn var-name [v]
  (-> v meta :name str))

(defn advance-help-list [app ns token forward?]
  (let [local-ns (symbol ns)
        help-list (app :completion-list)]
    (if (not= token (@help-state :token))
      (do
        (swap! help-state assoc :token token)
        (.setListData help-list (Vector.))
        (when-lets [vars (ns-vars local-ns)
                    best-vars (sort-by var-name
                                (filter
                                  #(.startsWith (var-name %) token)
                                  vars))
                    other-vars (sort-by var-name
                                 (filter 
                                   #(.contains (.substring (var-name %) 1) token)
                                   vars))]
                   (.setListData help-list (Vector. (concat best-vars other-vars)))
                   (.setSelectedIndex help-list 0)
                   ))
      (let [n (list-size help-list)]
        (when (pos? n)
          (.setSelectedIndex help-list
                             (clock-num
                               ((if forward? inc dec)
                                    (.getSelectedIndex help-list))
                               n)))))
    (when (pos? (list-size help-list))
      (set-first-component (app :repl-split-pane)
                           (app :help-text-scroll-pane))
      (set-first-component (app :doc-split-pane)
                           (app :completion-scroll-pane))
      (.ensureIndexIsVisible help-list
                             (.getSelectedIndex help-list)))))
  
(defn get-list-token [app]
  (-> app :completion-list .getSelectedValue var-name))

(defn show-help-text [app choice]
  (let [help-text (or (when choice (var-help choice)) "")]
    (.setText (app :help-text-area) help-text))
  (-> app :help-text-scroll-pane .getViewport (.setViewPosition (Point. (int 0) (int 0))))
  (swap! help-state assoc :visible true))

(defn show-tab-help [app text-comp forward?]
  (awt-event
    (let [ns (condp = text-comp
               (app :doc-text-area) (get-file-ns app)
               (app :repl-in-text-area) (get-repl-ns app))
          text (.getText text-comp)
          pos (.getCaretPosition text-comp)
          [start stop] (local-token-location text pos)]
      (when-let [token (.substring text start stop)]
        (swap! help-state assoc :pos start)
        (advance-help-list app ns token forward?)))))

(defn hide-tab-help [app]
  (awt-event
    (when (@help-state :visible)
      (set-first-component (app :repl-split-pane)
                           (app :repl-out-scroll-pane))
      (set-first-component (app :doc-split-pane)
                           (app :docs-tree-scroll-pane))
      (swap! help-state assoc :visible false :pos nil))))
  
(defn help-handle-caret-move [app text-comp]
  (let [[start _] (local-token-location (.getText text-comp) 
                                        (.getCaretPosition text-comp))]
    (if-not (= start (@help-state :pos))
      (hide-tab-help app)
      (show-tab-help app text-comp true))))

(defn update-token [app text-comp]
  (awt-event
    (let [[start stop] (local-token-location
                         (.getText text-comp)
                         (.getCaretPosition text-comp))
          len (- stop start)
          new-token (get-list-token app)]
      (when (and (not (empty? new-token)) (-> app :completion-list
                                              .getModel .getSize pos?))
        (.. text-comp getDocument
            (replace start len new-token nil))))))

(defn setup-tab-help [app text-comp]
  (attach-action-keys text-comp
    ["TAB" #(show-tab-help app text-comp true)]
    ["shift TAB" #(show-tab-help app text-comp false)]
    ["ESCAPE" #(hide-tab-help app)])
  (attach-child-action-keys text-comp
    ["ENTER" #(-> help-state deref :visible) #(update-token app text-comp)]))

(defn find-focused-text-pane [app]
  (let [t1 (app :doc-text-area)
        t2 (app :repl-in-text-area)]
    (cond (.hasFocus t1) t1
          (.hasFocus t2) t2)))

(defn present-var [v]
  (str (var-name v) " [" (-> v meta :ns) "]"))

(defn setup-completion-list [l app]
  (doto l
    (.setBackground (Color. 0xFF 0xFF 0xE8))
    (.setFocusable false)
    (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
    (.setCellRenderer
      (proxy [DefaultListCellRenderer] []
        (getListCellRendererComponent [list var index isSelected cellHasFocus]
          (doto (proxy-super getListCellRendererComponent list var index isSelected cellHasFocus)
            (.setText (present-var var)))))) 
    (.addListSelectionListener
      (reify ListSelectionListener
        (valueChanged [_ e]
          (when-not (.getValueIsAdjusting e)
            (.ensureIndexIsVisible l (.getSelectedIndex l))
            (show-help-text app (.getSelectedValue l))))))
    (on-click 2 #(when-let [text-pane (find-focused-text-pane app)]
                        (update-token app text-pane)))))
