(ns pge.core
  (:require #_[cljfx.api :as fx]
            [quil.applet :as a]
            [quil.core :as q]
            [quil.sketch :as sketch]
            [quil.middleware :as m])
  (:import #_[javafx.scene.canvas Canvas]
           [javafx.scene.paint Color]))

(def colors
  {:color/black Color/BLACK})

(defn color [pixel]
  (if (keyword? pixel)
    (colors pixel)
    (let [[r g b a] pixel]
      (Color/rgb r g b (or a 1)))))

;; (defn pge-canvas [{:keys [pixels]
;;                    {:screen/keys [width height] :as settings} :settings}]
;;   {:fx/type :canvas
;;    :width width
;;    :height height
;;    :draw (fn [^Canvas canvas]
;;            #_(let [writer (.getPixelWriter (.getGraphicsContext2D canvas))]
;;              (doseq [x (range width)
;;                      y (range height)
;;                      :let [i (+ y (* width x))
;;                            pixel (nth pixels i :color/black)]]
;;                (.setColor writer x y (color pixel))))
;;            (let [graphics (.getGraphicsContext2D canvas)]
;;              (.clearRect graphics 0 0 (:window/width settings) (:window/height settings))
;;              (doseq [x (range width)
;;                      y (range height)
;;                      :let [i (+ y (* width x))
;;                            pixel (nth pixels i :color/black)]]
;;                (.setFill graphics (color pixel))
;;                (.fillRect graphics x y (:pixel/width settings) (:pixel/height settings)))))})

#_(defn construct
  "Constructs a blank screen of the given width/height."
  [{:keys [setup-fn] :as args}]
  (let [settings (-> args
                     (dissoc :setup-fn)
                     (assoc :window/width (* (:screen/width args) (:pixel/width args))
                            :window/height (* (:screen/width args) (:pixel/width args))))]
    {:settings settings
     :state (atom (setup-fn settings))
     :renderer (fx/create-renderer
                :middleware
                (fx/wrap-map-desc
                 (fn [{:keys [pixels]}]
                   {:fx/type :stage
                    :showing true
                    :width (:window/width settings)
                    :height (:window/height settings)
                    :scene {:fx/type :scene
                            :root {:fx/type :v-box
                                   :children [{:fx/type pge-canvas
                                               :settings settings
                                               :pixels pixels}]}}})))}))

;; (defn start
;;   "Mounts the given renderer."
;;   [{:keys [state renderer] :as pge}]
;;   (fx/mount-renderer state renderer)
;;   pge)

(def pixel-coords
  (memoize
   (fn [{{:screen/keys [width height]} :settings}]
     (for [x (range width)
           y (range height)]
       [x y]))))

(defn- setup [on-create settings]
  (prn "setting up" on-create settings)
  ;; TODO: frame-rate locking
  (q/frame-rate 140)
  (q/background 255)
  (q/no-stroke)
  {:settings settings
   :pge (if on-create (on-create settings) {})})

(def ^:dynamic *updates* nil)

(defn draw [x y r g b]
  (swap! *updates* conj {:x x :y y :color [r g b]}))

(defn- do-draw [{{:pixel/keys [width height]} :settings
                 :keys [updates]}]
  (prn [:current (q/current-frame-rate) :target (q/target-frame-rate)])
  #_(q/ellipse (:x state) (:y state) (:r state) (:r state))
  (doseq [{:keys [x y color]} updates]
    (apply q/fill color)
    (q/rect (* width x) (* height y) width height)))

(defn- do-update [state on-update]
  (binding [*updates* (atom [])]
    (assoc (on-update state)
           :updates @*updates*)))

(defn construct-and-start! [{:keys [title on-create on-update] :as args}]
  (let [settings (-> args
                     (dissoc :setup-fn)
                     (assoc :window/width (* (:screen/width args) (:pixel/width args))
                            :window/height (* (:screen/width args) (:pixel/width args))))]
    {:settings settings
     :sketch (a/applet
                :size [(:window/width settings) (:window/height settings)]
                :host title
                :renderer :fx2d
                :setup #(setup on-create settings)
                :update #(do-update %1 (or on-update identity))
                :draw do-draw
                :middleware [m/fun-mode])}))
CCCC
