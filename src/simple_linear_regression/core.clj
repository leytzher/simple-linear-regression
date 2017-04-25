(ns simple-linear-regression.core
  (:require [clojure.math.numeric-tower :as math]
            [com.hypirion.clj-xchart :as plt]))


(defn mean [vector-of-numbers]
  (/ (apply + vector-of-numbers) (count vector-of-numbers)))

(defn square [x] (* x x))

(defn b1 [x y]
  (/
   (apply +
          (map *
               (map (fn [v] (- v (mean x))) x)
               (map (fn [v] (- v (mean y))) y)))
   (apply +
          (map (fn [v] (square (- v (mean x)))) x))))

(defn b0 [x y b1]
  (- (mean y)
     (* b1 (mean x))))

(defn linear-regression [x y]
  (let [b1 (b1 x y)
        b0 (b0 x y b1)]
    (into [] (map (fn [x-val] (+ (* b1 x-val) b0)) x))
    ))


(defn rmse [y pred]
  (let [n (count y)]
    (math/sqrt
     (/
      (apply + (map square (map - y pred)))
      n))
    ))


(defn -main []

  (def dataset {:x [1 2 4 3 5]
                :y [1 3 3 2 5]})

  (def predicted (linear-regression (:x dataset) (:y dataset)))

;;; plot results
  (plt/view
   (plt/xy-chart
    {"Data" {:x (:x dataset)
             :y (:y dataset)
             :style {:render-style :scatter}}
     "Prediction" {:x (:x dataset)
                   :y predicted
                   :style {:render-style :line
                           :line-color :red
                           :marker-type :none}}}
    {:title "Simple Linear Regression"
     :x-axis {:title "x"
              :min 0
              :max 6}
     :y-axis {:title "y"
              :min 0
              :max 6}}
    ))

  (println "RMSE: " (rmse (:y dataset) predicted))
  )


