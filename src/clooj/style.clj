(ns clooj.style
  (:import (javax.swing.text SimpleAttributeSet StyleConstants)
           (java.awt Color Font FontMetrics GraphicsEnvironment)
           (java.awt.image BufferedImage)))

(def graphics-object
  (memoize (fn [] (.createGraphics
                    (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB)))))

(defn set-text-color [text-comp offset length color]
  (let [style-doc (.getStyledDocument text-comp)
        attrs (SimpleAttributeSet.)]
     (StyleConstants/setForeground attrs color)
     (.setCharacterAttributes style-doc offset length attrs true)))

(defn get-tokens [txt]
  (re-seq #"[:|a-z|A-Z|/|\\|\.|-|0-9|\+\"]+" txt))

;; fonts

(def monospaced?
  (memoize
    (fn [font]
      (let [g (graphics-object)
            m (.getFontMetrics g font)]
        (apply == (map #(.charWidth m %) [\m \n \. \M \-]))))))

(defn get-all-fonts-12 []
  (let [font-names (.. GraphicsEnvironment
                       getLocalGraphicsEnvironment
                       getAvailableFontFamilyNames)]
    (map #(Font. % Font/PLAIN 12) font-names)))

(def get-monospaced-fonts []
  (filter monospaced? (get-all-fonts-12)))