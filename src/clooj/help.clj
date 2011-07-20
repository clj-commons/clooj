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

(defn var-source [ns v]
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

(defn var-help [ns v]
  (when-let [m (meta v)]
    (let [d (:doc m)
          s  (or (:clooj/src m)
                 (var-source ns v))]
       (str (:name m) "\n"
            (:arglists m) "\n"
            d "\n\n"
            (when (and d s)
              (.replace s d "...docs..."))))))
  
(defn current-form-string [text-area]
  (let [[left right] (find-enclosing-brackets
                       (.getText text-area)
                       (.getCaretPosition text-area))
        length (if (and right left) (inc (- right left)))]
    (when (and length (pos? length) (pos? left))
      (.getText text-area left (inc (- right left))))))

(defn head-var [ns form-string]
  (->> form-string read-string first (ns-resolve ns)))

(defn form-help [ns form-string]
   (var-help ns (head-var ns form-string)))