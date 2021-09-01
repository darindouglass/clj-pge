(defproject douglass/clj-psql "0.1.0"
  :description "An Clojure implementation of the oclPixelGameEngine"
;;  :license "EPL "
  :url "https://github.com/DarinDouglass/clj-pge"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [cljfx/cljfx "1.7.0"]
                 [quil/quil "3.1.0"]]
  :main pge.core
  :profiles {:dev {:source-paths ["dev"]}})
