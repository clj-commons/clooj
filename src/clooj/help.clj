(ns clooj.help
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT Reflector))
  (:use [clooj.brackets :only (find-enclosing-brackets)]))

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
  (let [[left right] (find-enclosing-brackets text pos)
        length (if (and right left) (inc (- right left)))]
    (when (and length (pos? length) (pos? left))
      (.substring text left (inc right)))))

(defn head-symbol [form-string]
  (->> form-string read-string first))

(defn form-help [ns form-string]
  (var-help (ns-resolve ns (head-symbol form-string))))

(defn var-arglist-text [var]
  (->> var meta :arglists (str (name var) ": ")))