(ns pge.draw.line
  (:require [pge.binary :as b]
            [pge.math :as m]))

(defn bresenham-pixels
  [x1 y1 x2 y2]
  (let [steep? (> (Math/abs (- y1 y2)) (Math/abs (- x1 x2)))
        [x1 y1 x2 y2] (if steep? [y1 x1 y2 x2] [x1 y1 x2 y2])
        [x1 y1 x2 y2] (if (> x1 x2) [x2 y2 x1 y1] [x1 y1 x2 y2])
        delta-x (- x2 x1)
        delta-y (Math/abs (- y1 y2))
        y-step (if (< y1 y2) 1 -1)
        plot (fn [x y]
               {:x (if steep? y x)
                :y (if steep? x y)})]
    (loop [pixels []
           x x1
           y y1
           error (Math/floor (/ delta-x 2))]
      (let [new-pixels (cons (plot x y) pixels)]
        (if (>= x x2)
          new-pixels
          (if (< error delta-y)
            (recur new-pixels (inc x) (+ y y-step) (+ error (- delta-x delta-y)))
            (recur new-pixels (inc x) y (- error delta-y))))))))

(def font-data
  (let [char-code #(- (.charCodeAt %) 48)
      sheet (str "?Q`0001oOch0o01o@F40o0<AGD4090LAGD<090@A7ch0?00O7Q`0600>00000000"
                 "O000000nOT0063Qo4d8>?7a14Gno94AA4gno94AaOT0>o3`oO400o7QN00000400"
                 "Of80001oOg<7O7moBGT7O7lABET024@aBEd714AiOdl717a_=TH013Q>00000000"
                 "720D000V?V5oB3Q_HdUoE7a9@DdDE4A9@DmoE4A;Hg]oM4Aj8S4D84@`00000000"
                 "OaPT1000Oa`^13P1@AI[?g`1@A=[OdAoHgljA4Ao?WlBA7l1710007l100000000"
                 "ObM6000oOfMV?3QoBDD`O7a0BDDH@5A0BDD<@5A0BGeVO5ao@CQR?5Po00000000"
                 "Oc``000?Ogij70PO2D]??0Ph2DUM@7i`2DTg@7lh2GUj?0TO0C1870T?00000000"
                 "70<4001o?P<7?1QoHg43O;`h@GT0@:@LB@d0>:@hN@L0@?aoN@<0O7ao0000?000"
                 "OcH0001SOglLA7mg24TnK7ln24US>0PL24U140PnOgl0>7QgOcH0K71S0000A000"
                 "00H00000@Dm1S007@DUSg00?OdTnH7YhOfTL<7Yh@Cl0700?@Ah0300700000000"
                 "<008001QL00ZA41a@6HnI<1i@FHLM81M@@0LG81?O`0nC?Y7?`0ZA7Y300080000"
                 "O`082000Oh0827mo6>Hn?Wmo?6HnMb11MP08@C11H`08@FP0@@0004@000000000"
                 "00P00001Oab00003OcKP0006@6=PMgl<@440MglH@000000`@000001P00000000"
                 "Ob@8@@00Ob@8@Ga13R@8Mga172@8?PAo3R@827QoOb@820@0O`0007`0000007P0"
                 "O`000P08Od400g`<3V=P0G`673IP0`@3>1`00P@6O`P00g`<O`000GP800000000"
                 "?P9PL020O`<`N3R0@E4HC7b0@ET<ATB0@@l6C4B0O`H3N7b0?P01L3R000000020")
      counter (atom -1)
      coords (fn []
               (let [c (swap! counter inc)]
                 {:x (quot c 48)
                  :y (mod c 48)}))]
  (reduce (fn [all [a b c d]]
            (let [bytes (b/or (b/<< (char-code a) 18)
                              (b/<< (char-code b) 12)
                              (b/<< (char-code c) 6)
                              (char-code d))]
              (merge all
                     (reduce #(assoc %1 (coords) (pos? (b/and bytes (b/<< 1 %2))))
                             {}
                             (range 24)))))
          {}
          (partition 4 sheet))))
