; Copyright (c) 2011-2013, Arthur Edelstein
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
           (javax.swing.event ListSelectionListener)
           (java.io File))
  (:require [clojure.repl]
            [clojure.string :as string]
            [clooj.collaj :as collaj]
            [clooj.utils :as utils]
            [clooj.brackets :as brackets]
            [cemerick.pomegranate.aether :as aether]
            [clj-inspector.jars :as jars]
            [clj-inspector.vars :as vars]))

(def var-maps-agent (agent nil))

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

(defn present-item [item]
  (str (:name item) " [" (:ns item) "]"))

(defn make-var-super-map [var-maps]
  (into {}
        (for [var-map var-maps]
          [[(:ns var-map) (:name var-map)] var-map])))

(defn classpath-to-jars [project-path classpath]
  (apply concat
    (for [item classpath]
      (cond (.endsWith item "*") (jars/jar-files (apply str (butlast item)))
            (.endsWith item ".jar") (list (File. item))
            :else (jars/jar-files item)))))

(defn get-sources-from-jars [project-path classpath]
   (->> (classpath-to-jars project-path classpath)
       (mapcat jars/clj-sources-from-jar)
       merge
       vals))

(defn get-sources-from-clj-files [classpath]
  (map slurp
       (apply concat
              (for [item classpath]
                (let [item-file (File. item)]
                  (when (.isDirectory item-file)
                    (filter #(.endsWith (.getName %) ".clj")
                            (file-seq item-file))))))))

(defn get-var-maps [project-path classpath]
  (make-var-super-map
      (mapcat #(vars/analyze-clojure-source "clj" %)
              (concat
                (get-sources-from-jars project-path classpath)
                (get-sources-from-clj-files classpath)))))

(defn update-var-maps! [project-path classpath]
  (send-off var-maps-agent #(merge % (get-var-maps project-path classpath))))

(defn find-form-string [text pos]
  (let [[left right] (brackets/find-enclosing-brackets text pos)]
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

(defn current-ns-form [app]
  (-> app :doc-text-area .getText read-string))

(defn ns-available-names [app]
  (vars/parse-ns-form (current-ns-form app)))

(defn arglist-from-var-map [m]
  (or
    (when-let [args (:arglists m)]
      (str (-> m :ns) "/" (:name m) ": " args))
    ""))

(defn token-from-caret-pos [text pos]
  (head-token (find-form-string text pos)))

(defn var-from-token [app current-ns token]
  (when token
    (if (.contains token "/")
      (vec (.split token "/"))
      (or ((ns-available-names app) token)
          [current-ns token]))))

(defn arglist-from-token [app ns token]
  (or (special-forms token)
      (when-let [repl (:repl app)]
        (-> @var-maps-agent (get (var-from-token app ns token))
            arglist-from-var-map))))

(defn arglist-from-caret-pos [app ns text pos]
  (let [token (token-from-caret-pos text pos)]
    (arglist-from-token app ns token)))

;; tab help


(defonce help-state (atom {:visible false :token nil :pos nil}))

(defn var-map [v]
  (when-let [m (meta v)]
    (let [ns (:ns m)]
      (-> m
          (select-keys [:doc :ns :name :arglists])
          (assoc :source (binding [*ns* ns]
                           (clojure.repl/source-fn (symbol (str ns "/" name)))))))))

(defn var-help [var-map]
  (let [{:keys [doc ns name arglists source]} var-map]
    (str name
         (if ns (str " [" ns "]") "") "\n"
         arglists
         "\n\n"
         (if doc
           (str "Documentation:\n" doc)
           "No documentation found.")
         "\n\n"
         (if source
           (str "Source:\n"
                (if doc
                  (.replace source doc "...docs...")
                  source))
           "No source found."))))

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
           [(present-item c) "\n  java class"]
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
  (cond (map? item) (var-help item)
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

(defn match-items [pattern items]
  (->> items
    (filter #(re-find pattern (:name %)))
    (sort-by #(.toLowerCase (:name %)))))

(defn hits [token]
  (let [token-pat1 (re-pattern (str "(?i)\\A\\Q" token "\\E"))
        token-pat2 (re-pattern (str "(?i)\\A.\\Q" token "\\E"))
        items (vals @var-maps-agent)
        best (match-items token-pat1 items)
        others (match-items token-pat2 items)
        ;collaj-items (or (try (collaj/raw-data token) (catch Throwable _)))
        ]
    (concat best others #_collaj-items)))

(defn show-completion-list [{:keys [completion-list
                                    repl-split-pane
                                    help-text-scroll-pane
                                    doc-split-pane
                                    completion-panel
                                    repl-label]:as app}]
    (when (pos? (list-size completion-list))
      (set-first-component repl-split-pane help-text-scroll-pane)
      (set-first-component doc-split-pane completion-panel)
      (.setText repl-label "Documentation")
      (.ensureIndexIsVisible completion-list
                             (.getSelectedIndex completion-list))))

(defn advance-help-list [app token index-change-fn]
  (let [help-list (app :completion-list)]
    (if (not= token (@help-state :token))
      (do
        (swap! help-state assoc :token token)
        (.setListData help-list (Vector. (hits token)))
        (.setSelectedIndex help-list 0))
      (let [n (list-size help-list)]
        (when (pos? n)
          (.setSelectedIndex help-list
                             (clock-num
                               (index-change-fn
                                    (.getSelectedIndex help-list))
                               n))))))
  (show-completion-list app))
  
(defn get-list-item [app]
  (-> app :completion-list .getSelectedValue))

(defn get-list-artifact [app]
  (when-let [artifact (:artifact (get-list-item app))]
    (binding [*read-eval* false] 
      (read-string artifact))))

(defn get-list-token [app]
  (let [val (get-list-item app)]
    (str (:ns val) "/" (:name val))))

(defn show-help-text [app choice]
  (let [help-text (or (when choice (item-help choice)) "")]
    (.setText (app :help-text-area) help-text))
  (-> app :help-text-scroll-pane .getViewport
      (.setViewPosition (Point. (int 0) (int 0)))))

(defn show-tab-help [app text-comp index-change-fn]
  (utils/awt-event
    (let [text (utils/get-text-str text-comp)
          pos (.getCaretPosition text-comp)
          [start stop] (local-token-location text pos)]
      (when-let [token (.substring text start stop)]
        (swap! help-state assoc :pos start :visible true)
        (advance-help-list app token index-change-fn)))))

(defn hide-tab-help [app]
  (utils/awt-event
    (when (@help-state :visible)
      (set-first-component (app :repl-split-pane)
                           (app :repl-out-scroll-pane))
      (set-first-component (app :doc-split-pane)
                           (app :docs-tree-panel))
      (.setText (app :repl-label) "Clojure REPL output"))
    (swap! help-state assoc :visible false :pos nil)))
  
(defn help-handle-caret-move [app text-comp]
  (utils/awt-event
    (when (@help-state :visible)
      (let [[start _] (local-token-location (utils/get-text-str text-comp) 
                                            (.getCaretPosition text-comp))]
        (if-not (= start (@help-state :pos))
          (hide-tab-help app)
          (show-tab-help app text-comp identity))))))

(defn update-ns-form [app]
  (current-ns-form app))

(defn add-classpath-to-repl
  [app files]
  (.addAll (app :classpath-queue) files))

(defn load-dependencies [app artifact]
  (utils/awt-event (utils/append-text (app :repl-out-text-area)
               (str "\nLoading " artifact " ... ")))
  (let [deps (cemerick.pomegranate.aether/resolve-dependencies
               :coordinates [artifact]
               :repositories
                 (merge aether/maven-central
                        {"clojars" "http://clojars.org/repo"}))]
    (add-classpath-to-repl app (aether/dependency-files deps)))
  (utils/append-text (app :repl-out-text-area)
                     (str "done.")))
  
(defn update-token [app text-comp new-token]
  (utils/awt-event
    (let [[start stop] (local-token-location
                         (utils/get-text-str text-comp)
                         (.getCaretPosition text-comp))
          len (- stop start)]
      (when (and (seq new-token) (-> app :completion-list .getModel .getSize pos?))
        (.. text-comp getDocument
            (replace start len new-token nil))))))

(defn setup-tab-help [text-comp app]
  (utils/attach-action-keys text-comp
    ["TAB" #(show-tab-help app text-comp inc)]
    ["shift TAB" #(show-tab-help app text-comp dec)]
    ["ESCAPE" #(hide-tab-help app)])
  (utils/attach-child-action-keys text-comp
    ["ENTER" #(@help-state :visible)
             #(do (hide-tab-help app)
                  (.start (Thread. (fn [] (load-dependencies app (get-list-artifact app)))))
                  (update-token app text-comp (get-list-token app)))]))

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
            (.setText (present-item item)))))) 
    (.addListSelectionListener
      (reify ListSelectionListener
        (valueChanged [_ e]
          (when-not (.getValueIsAdjusting e)
            (.ensureIndexIsVisible l (.getSelectedIndex l))
            (show-help-text app (.getSelectedValue l))))))
    (utils/on-click 2 #(when-let [text-pane (find-focused-text-pane app)]
                        (update-token app text-pane (get-list-token app))))))
