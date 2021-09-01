(ns pge.core
  (:require [pge.draw.line :as line]))

(def default-opts
  {})

(def colors ["#FF0000"
             "#00FF00"
             "#0000FF"
             "#00FFFF"
             "#FFFF00"
             "#FF00FF"])

;; Drawing
(defn draw
  ([{{:keys [context pixel-width pixel-height]} :pge/meta :as pge} {:keys [x y color]}]
   (when (and x y color)
     (println x y color)
     (set! (.-fillStyle context) color)
     (.fillRect context (* x pixel-width) (* y pixel-height) pixel-width pixel-height))
   pge)
  ([pge x y color]
   (draw pge {:x x :y y :color color})))

(defn draw-line
  "Draw a line according to Bresenham's line algorithm."
  [pge x1 y1 x2 y2 color]
  (let [steep? (> (Math/abs (- y1 y2)) (Math/abs (- x1 x2)))
        [x1 y1 x2 y2] (if steep? [y1 x1 y2 x2] [x1 y1 x2 y2])
        [x1 y1 x2 y2] (if (> x1 x2) [x2 y2 x1 y1] [x1 y1 x2 y2])
        delta-x (- x2 x1)
        delta-y (Math/abs (- y1 y2))
        y-step (if (< y1 y2) 1 -1)]
    (loop [x x1
           y y1
           error (Math/floor (/ delta-x 2))]
      (draw pge (if steep? y x) (if steep? x y) color)
      (when (< x x2)
        (if (< error delta-y)
          (recur (inc x) (+ y y-step) (+ error (- delta-x delta-y)))
          (recur (inc x) y (- error delta-y)))))
    pge))

(defn draw-circle
  "Draw a circle according to Bresenham's circle algorithm."
  [pge center-x center-y radius color]
  (let [draw-octants #(doseq [[x y] [[%1 %2]
                                     [%2 %1]]
                              [x-factor y-factor] [[1 1]
                                                   [-1 1]
                                                   [1 -1]
                                                   [-1 -1]]]
                        (draw pge
                              (+ center-x (* x-factor x))
                              (+ center-y (* y-factor y))
                              color))]
    (loop [x 0
           y radius
           d (- 3 (* 2 radius))]
      (draw-octants x y)
      (when (>= y x)
        (recur (inc x)
               (if (pos? d) (dec y) y)
               (if (pos? d)
                 (+ d 10 (* 4 (- x y)))
                 (+ d 6 (* 4 x))))))))

;; TODO: simplify this by making `line/font-data` into a map
;;       from `char -> data`.
(defn draw-char
  ([pge x y char color]
   (draw-char pge x y char color {}))
  ([pge x y char color {:keys [scale] :or {scale 1}}]
   (let [code (.charCodeAt char 0)
         x-offset (* 8 (mod (- code 32) 16))
         y-offset (* 8 (quot (- code 32) 16))]
     (doseq [i (range 64)
             :let [font-x (quot i 8)
                   font-y (mod i 8)]]
       (when (line/font-data {:x (+ font-x x-offset) :y (+ font-y y-offset)})
         (doseq [scale-offset-x (range scale)
                 scale-offset-y (range scale)]
           (draw pge
                 (+ x (* scale font-x) scale-offset-x)
                 (+ y (* scale font-y) scale-offset-y)
                 color)))))))

(defn draw-string
  ([pge x y string color]
   (draw-string pge x y string color {}))
  ([pge x y string color {:keys [scale] :or {scale 1}}]
   (loop [[char & rest] string
          start-x x
          start-y y]
     (if (= "\n" char)
       ;; Move one row down
       (recur rest x (+ start-y (* 8 scale)))
       (do
         (draw-char pge start-x start-y char color {:scale scale})
         (when-not (empty? rest)
           (recur rest (+ start-x (* 8 scale)) start-y)))))
   pge))

(defn draw-rect [pge x y width height color]
  (-> pge
      (draw-line x y (+ x width) y color)
      (draw-line (+ x width) y (+ x width) (+ y height) color)
      (draw-line (+ x width) (+ y height) x (+ y height) color)
      (draw-line x (+ y height) x y color)))

(defn fill-rect [pge x y width height color]
  (doseq [h (range height)]
    (draw-line pge x (+ y h) (+ x width) (+ y h) color))
  pge)

(defn draw-triangle [pge x1 y1 x2 y2 x3 y3 color]
  (-> pge
      (draw-line x1 y1 x2 y2 color)
      (draw-line x2 y2 x3 y3 color)
      (draw-line x3 y3 x1 y1 color)))

(defn clear
  ([pge]
   (clear pge "#FFF"))
  ([{{:keys [context screen-width screen-height]} :pge/meta :as pge} color]
   (println screen-width screen-height)
   (set! (.-fillStyle context) color)
   (.fillRect context 0 0 screen-width screen-height)
   pge))

;; Lifecycle
(defn- setup-canvas [id width height]
  (let [canvas (.getElementById js/document id)]
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)
    (.getContext canvas "2d")))

(defn construct
  ([window-width window-height pixel-width pixel-height opts]
   (construct {} window-width window-height pixel-width pixel-height opts))
  ([state window-width window-height pixel-width pixel-height {:keys [id] :as opts}]
   (println window-width window-height pixel-width pixel-height)
   (let [window-width (inc window-width)
         window-height (inc window-height)
         screen-widthCCCCC (* window-width pixel-width)
         screen-height (* window-height pixel-height)]
     (println window-width window-height pixel-width pixel-height screen-width screen-height)
     (->> opts
          (merge default-opts
                 {:context (setup-canvas id screen-width screen-height)
                  :window-width window-width
                  :window-height window-height
                  :pixel-width pixel-width
                  :pixel-height pixel-height
                  :screen-width screen-width
                  :screen-height screen-height})
          (assoc state :pge/meta)))))

(defn start [pge on-update]
  (let [stop? (atom false)
        state (atom (assoc-in pge [:pge/meta :stop?] stop?))
        last-run (atom nil)]
    (letfn [(render [timestamp]
              (when-not @stop?
                (let [elapsed-ms (- timestamp (or @last-run timestamp))]
                  (.requestAnimationFrame js/window render)
                  (try
                    (let [next-pge (on-update @state elapsed-ms)]
                      (reset! state next-pge))
                    (catch js/Error e
                      (prn e)
                      (reset! stop? true)
                      (println e))))))]
      (.requestAnimationFrame js/window render)
      state)))

(defn stop [pge]
  (let [{{:keys [stop?]} :pge/meta} @pge]
    (reset! stop? true)))
