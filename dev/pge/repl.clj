(ns pge.repl
  (:require [pge.core :as pge]))

;; (defn setup [{:screen/keys [width height] :as pge}]
;;   (prn "HERE")
;;   (let [x (reduce (fn [pge [x y]]
;;                     (pge/draw pge x y (rand-int 256) (rand-int 256) (rand-int 256)))
;;                   pge
;;                   (for [x (range width)
;;                         y (range height)]
;;                     [x y]))]
;;     x))

(defn update [state]
  (->> state
       (pge/pixel-coords)
       (map (fn [[x y]]
              (pge/draw x y (rand-int 256) (rand-int 256) (rand-int 256))))
       (doall))
  state)

(comment
  (def pge (pge/construct-and-start! {:screen/width 16
                                      :screen/height 16
                                      :pixel/width 4
                                      :pixel/height 4
                                      :on-update update})))
