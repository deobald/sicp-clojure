(ns sec1-3.ex1-44
  (:use [sec1-3.ex1-43 :only (repeated)]))

(defn sq [num]
  (* num num))

(defn avg [coll]
  (/ (apply + coll) (count coll)))

(defn smooth [f]
  (let [dx 0.00001]
    #(avg [(f (- % dx)) (f %) (f (+ % dx))])))

(defn smoothed [f times]
  (let [done (repeated smooth times)]
    (done f )))

(def gaz repeated)

((smoothed sq 2) 6)