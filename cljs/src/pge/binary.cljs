(ns pge.binary
  (:refer-clojure :exclude [or and]))

(defn and [& args]
  (apply bit-and args))

(defn or [& args]
  (apply bit-or args))

(defn << [bits num]
  (bit-shift-left bits num))
