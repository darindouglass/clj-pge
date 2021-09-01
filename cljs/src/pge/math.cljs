(ns pge.math)

(defn abs [x]
  (if (neg? x)
    (- x)
    x))
